import os
import argparse
import re
import requests
import concurrent.futures
from rich.progress import Progress, SpinnerColumn, TextColumn, BarColumn, TimeElapsedColumn
from rich.console import Console

session = requests.Session()
session.headers.update({'User-Agent': 'Mozilla/5.0 (Maintainer update script)'})
console = Console()

def parse_version(v_str):
    return tuple(int(x) for x in re.findall(r'\d+', v_str))

def check_url(url):
    try:
        response = session.head(url, allow_redirects=True, timeout=5)
        return response.status_code in (200, 302)
    except requests.RequestException:
        return False

def scrape_directory_index(url, current_v_str, prefix, suffix):
    base_url = url.rsplit('/', 1)[0] + '/'
    try:
        resp = session.get(base_url, timeout=5)
        if resp.status_code != 200:
            return None
            
        hrefs = re.findall(r'href="([^"]+)"', resp.text)
        best_v_tuple = parse_version(current_v_str)
        best_url = None
        
        for href in hrefs:
            clean_href = href.split('#')[0].split('?')[0]
            if clean_href.startswith(prefix) and clean_href.endswith(suffix):
                v_match = re.search(r'(\d+\.\d+(?:\.\d+)*)', clean_href)
                if v_match:
                    test_v_str = v_match.group(1)
                    test_v_tuple = parse_version(test_v_str)
                    if test_v_tuple > best_v_tuple:
                        best_v_tuple = test_v_tuple
                        best_url = base_url + clean_href if not clean_href.startswith('http') else clean_href
                        
        return best_url
    except requests.RequestException:
        return None

def heuristic_climb(best_url, current_v_str):
    parts = list(parse_version(current_v_str))
    found_newer = True
    
    def generate_candidates(p):
        if len(p) >= 3:
            return [[p[0], p[1], p[2] + 1], [p[0], p[1] + 1, 0], [p[0] + 1, 0, 0]]
        elif len(p) == 2:
            return [[p[0], p[1] + 1], [p[0] + 1, 0]]
        return []

    while found_newer:
        found_newer = False
        for candidate_parts in generate_candidates(parts):
            new_v_str = ".".join(map(str, candidate_parts))
            test_url = best_url.replace(current_v_str, new_v_str)
            
            if len(candidate_parts) >= 2 and len(parts) >= 2:
                old_dir = f"/{parts[0]}.{parts[1]}/"
                new_dir = f"/{candidate_parts[0]}.{candidate_parts[1]}/"
                test_url = test_url.replace(old_dir, new_dir)
                
            squashed_old = current_v_str.replace('.', '')
            squashed_new = new_v_str.replace('.', '')
            if f"gs{squashed_old}" in test_url:
                test_url = test_url.replace(f"gs{squashed_old}", f"gs{squashed_new}")

            if check_url(test_url):
                best_url = test_url
                parts = candidate_parts
                current_v_str = new_v_str
                found_newer = True
                break
                
    return best_url

def upgrade_url(line):
    original_line = line.strip()
    if original_line.startswith('svn://') or original_line.endswith('.git'):
        return original_line

    filename = original_line.rsplit('/', 1)[-1]
    match = re.search(r'(\d+\.\d+(?:\.\d+)*)', filename)
    if not match:
        return original_line
        
    current_v_str = match.group(1)
    prefix = filename[:match.start()]
    suffix = filename[match.end():]
    
    scraped_url = scrape_directory_index(original_line, current_v_str, prefix, suffix)
    if scraped_url:
        original_line = scraped_url
        match = re.search(r'(\d+\.\d+(?:\.\d+)*)', scraped_url.rsplit('/', 1)[-1])
        current_v_str = match.group(1)
        
    final_url = heuristic_climb(original_line, current_v_str)
    return final_url


def process_file(input_filepath, output_filepath):
    with open(input_filepath, 'r') as f:
        urls = [line.strip() for line in f.readlines() if line.strip()]

    updated_urls = [None] * len(urls)
    
    console.print(f"\n[bold cyan]Démarrage de l'analyse de {len(urls)} paquets...[/bold cyan]\n")

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
        TimeElapsedColumn(),
        console=console,
        transient=False
    ) as progress:
        
        overall_task = progress.add_task("[bold blue]Progression globale", total=len(urls))
        
        def process_url_with_ui(idx, url):
            filename = url.rsplit('/', 1)[-1]
            
            task_id = progress.add_task(f"[yellow]En cours : {filename}", total=None)
            
            try:
                result = upgrade_url(url)
            finally:
                progress.remove_task(task_id)
            
            return idx, url, result

        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(process_url_with_ui, i, url) for i, url in enumerate(urls)]
            
            for future in concurrent.futures.as_completed(futures):
                idx, original, updated = future.result()
                updated_urls[idx] = updated
                
                filename = original.rsplit('/', 1)[-1]
                
                if original != updated:
                    new_filename = updated.rsplit('/', 1)[-1]
                    progress.console.print(f"[bold green]✓ MIS À JOUR[/bold green] : {filename} ➔ [bold white]{new_filename}[/bold white]")
                else:
                    progress.console.print(f"[dim]• À JOUR[/dim] : {filename}")
                    
                progress.advance(overall_task)

    with open(output_filepath, 'w') as f:
        for url in updated_urls:
            f.write(f"{url}\n")
            
    console.print(f"\n[bold green]Terminé ![/bold green] Le fichier a été sauvegardé sous : [u]{output_filepath}[/u]\n")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Met à jour les URLs de téléchargement des paquets AppImage.')
    parser.add_argument('input_file', help='Chemin vers le fichier d\'entrée contenant les URLs à analyser.')
    args = parser.parse_args()

    input_filepath = args.input_file
    process_file(input_filepath, "tmp.txt")
    os.replace("tmp.txt", input_filepath)