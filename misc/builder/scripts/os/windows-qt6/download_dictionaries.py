import os
import sys
import shutil
import tarfile
import urllib.request
import subprocess

BASE_URL = "https://ftp.texmacs.org/TeXmacs/misc/aspell/"

DICTIONARIES = [
    "af/aspell-af-0.50-0.tar.bz2",
    "am/aspell6-am-0.03-1.tar.bz2",
    "ar/aspell6-ar-1.2-0.tar.bz2",
    "ast/aspell6-ast-0.01.tar.bz2",
    "az/aspell6-az-0.02-0.tar.bz2",
    "be/aspell5-be-0.01.tar.bz2",
    "bg/aspell6-bg-4.1-0.tar.bz2",
    "bn/aspell6-bn-0.01.1-1.tar.bz2",
    "br/aspell-br-0.50-2.tar.bz2",
    "ca/aspell6-ca-2.1.5-1.tar.bz2",
    "cs/aspell6-cs-20040614-1.tar.bz2",
    "csb/aspell6-csb-0.02-0.tar.bz2",
    "cy/aspell-cy-0.50-3.tar.bz2",
    "da/aspell6-da-1.6.36-11-0.tar.bz2",
    "de/aspell6-de-20161207-7-0.tar.bz2",
    "de-alt/aspell6-de-alt-2.1-1.tar.bz2",
    "el/aspell6-el-0.08-0.tar.bz2",
    "en/aspell6-en-2020.12.07-0.tar.bz2",
    "eo/aspell6-eo-2.1.20000225a-2.tar.bz2",
    "es/aspell6-es-1.11-2.tar.bz2",
    "et/aspell6-et-0.1.21-1.tar.bz2",
    "fa/aspell6-fa-0.11-0.tar.bz2",
    "fi/aspell6-fi-0.7-0.tar.bz2",
    "fo/aspell5-fo-0.2.16-1.tar.bz2",
    "fr/aspell-fr-0.50-3.tar.bz2",
    "fy/aspell6-fy-0.12-0.tar.bz2",
    "ga/aspell5-ga-4.5-0.tar.bz2",
    "gd/aspell5-gd-0.1.1-1.tar.bz2",
    "gl/aspell6-gl-0.5a-2.tar.bz2",
    "grc/aspell6-grc-0.02-0.tar.bz2",
    "gu/aspell6-gu-0.03-0.tar.bz2",
    "gv/aspell-gv-0.50-0.tar.bz2",
    "he/aspell6-he-1.0-0.tar.bz2",
    "hi/aspell6-hi-0.02-0.tar.bz2",
    "hil/aspell5-hil-0.11-0.tar.bz2",
    "hr/aspell-hr-0.51-0.tar.bz2",
    "hsb/aspell6-hsb-0.02-0.tar.bz2",
    "hu/aspell6-hu-0.99.4.2-0.tar.bz2",
    "hus/aspell6-hus-0.03-1.tar.bz2",
    "hy/aspell6-hy-0.10.0-0.tar.bz2",
    "ia/aspell-ia-0.50-1.tar.bz2",
    "id/aspell5-id-1.2-0.tar.bz2",
    "is/aspell-is-0.51.1-0.tar.bz2",
    "it/aspell6-it-2.2_20050523-0.tar.bz2",
    "kn/aspell6-kn-0.01-1.tar.bz2",
    "ku/aspell5-ku-0.20-1.tar.bz2",
    "ky/aspell6-ky-0.01-0.tar.bz2",
    "la/aspell6-la-20020503-0.tar.bz2",
    "lt/aspell6-lt-1.2.1-0.tar.bz2",
    "lv/aspell6-lv-0.5.5-1.tar.bz2",
    "mg/aspell5-mg-0.03-0.tar.bz2",
    "mi/aspell-mi-0.50-0.tar.bz2",
    "mk/aspell-mk-0.50-0.tar.bz2",
    "ml/aspell6-ml-0.03-1.tar.bz2",
    "mn/aspell6-mn-0.06-2.tar.bz2",
    "mr/aspell6-mr-0.10-0.tar.bz2",
    "ms/aspell-ms-0.50-0.tar.bz2",
    "mt/aspell-mt-0.50-0.tar.bz2",
    "nb/aspell-nb-0.50.1-0.tar.bz2",
    "nds/aspell6-nds-0.01-0.tar.bz2",
    "nl/aspell-nl-0.50-2.tar.bz2",
    "nn/aspell-nn-0.50.1-1.tar.bz2",
    "ny/aspell5-ny-0.01-0.tar.bz2",
    "or/aspell6-or-0.03-1.tar.bz2",
    "pa/aspell6-pa-0.01-1.tar.bz2",
    "pl/aspell6-pl-6.0_20061121-0.tar.bz2",
    "pt_BR/aspell6-pt_BR-20131030-12-0.tar.bz2",
    "pt_PT/aspell6-pt_PT-20190329-1-0.tar.bz2",
    "qu/aspell6-qu-0.02-0.tar.bz2",
    "ro/aspell5-ro-3.3-2.tar.bz2",
    "ru/aspell6-ru-0.99f7-1.tar.bz2",
    "rw/aspell-rw-0.50-0.tar.bz2",
    "sc/aspell5-sc-1.0.tar.bz2",
    "sk/aspell6-sk-2.01-2.tar.bz2",
    "sl/aspell-sl-0.50-0.tar.bz2",
    "sr/aspell6-sr-0.02.tar.bz2",
    "sv/aspell-sv-0.51-0.tar.bz2",
    "sw/aspell-sw-0.50-0.tar.bz2",
    "ta/aspell6-ta-20040424-1.tar.bz2",
    "te/aspell6-te-0.01-2.tar.bz2",
    "tet/aspell5-tet-0.1.1.tar.bz2",
    "tk/aspell5-tk-0.01-0.tar.bz2",
    "tl/aspell5-tl-0.02-1.tar.bz2",
    "tn/aspell5-tn-1.0.1-0.tar.bz2",
    "tr/aspell-tr-0.50-0.tar.bz2",
    "uk/aspell6-uk-1.4.0-0.tar.bz2",
    "uz/aspell6-uz-0.6-0.tar.bz2",
    "vi/aspell6-vi-0.01.1-1.tar.bz2",
    "wa/aspell-wa-0.50-0.tar.bz2",
    "yi/aspell6-yi-0.01.1-1.tar.bz2",
    "zu/aspell-zu-0.50-0.tar.bz2"
]

def install_dictionaries():
    working_dir = os.getcwd()
    
    install_prefix = os.path.join(working_dir, "local")
    
    if not os.path.exists(install_prefix):
        os.makedirs(install_prefix)
        print(f"Created installation directory: {install_prefix}")

    print(f"Starting processing of {len(DICTIONARIES)} dictionaries...")
    print(f"Installation Prefix: {install_prefix}")

    failed_downloads = []

    for rel_path in DICTIONARIES:
        filename = rel_path.split("/")[-1]
        full_url = BASE_URL + filename
        
        print(f"\n--------------------------------------------------")
        print(f"Processing: {filename}")
        
        try:
            print(f"Downloading from {full_url}...")
            urllib.request.urlretrieve(full_url, filename)
            
            print("Extracting...")
            extracted_dirname = None
            
            with tarfile.open(filename, "r:bz2") as tar:
                tar.extractall()
                for member in tar.getmembers():
                    if member.isdir():
                        extracted_dirname = member.name.split('/')[0]
                        break
                
                if not extracted_dirname:
                     extracted_dirname = filename.replace(".tar.bz2", "")

            if not os.path.exists(extracted_dirname):
                print(f"Error: Could not find extracted directory {extracted_dirname}")
                continue

            original_path = os.getcwd()
            
            os.chdir(extracted_dirname)
            
            print(f"Configuring in {extracted_dirname}...")
            subprocess.check_call(["sh", "./configure"])
            
            print("Compiling (make)...")
            subprocess.check_call(["mingw32-make"])
            
            print("Installing (make install)...")
            subprocess.check_call(["mingw32-make", "install"])
            
            os.chdir(original_path)
            
            print("Cleaning up...")
            shutil.rmtree(extracted_dirname)
            os.remove(filename)
            print(f"Successfully installed {filename}")

        except urllib.error.URLError as e:
            failed_downloads.append(filename)
            print(f"Failed to download {filename}: {e}")
        except subprocess.CalledProcessError as e:
            failed_downloads.append(filename)
            print(f"Build failed for {filename}: {e}")
            if os.getcwd() != working_dir:
                os.chdir(working_dir)
            if os.path.exists(filename):
                os.remove(filename)
            if 'extracted_dirname' in locals() and extracted_dirname and os.path.exists(extracted_dirname):
                shutil.rmtree(extracted_dirname)
        except Exception as e:
            failed_downloads.append(filename)
            print(f"An unexpected error occurred for {filename}: {e}")
            if os.getcwd() != working_dir:
                os.chdir(working_dir)

    print("\n--------------------------------------------------")
    print("Batch processing complete.")
    if failed_downloads:
        print("\nThe following failed:")
        for failed in failed_downloads:
            print(f" - {failed}")

if __name__ == "__main__":
    install_dictionaries()