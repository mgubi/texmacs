<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|\<#C9\>crire des styles bibliographiques pour <TeXmacs>>

  <section|Introduction>

  <TeXmacs> g\<#E8\>re les bibliographies avec <BibTeX> ou avec un outil
  int\<#E9\>gr\<#E9\>. Les styles <BibTeX> sont d\<#E9\>sign\<#E9\>s par leur nom usuel ; les styles
  de <TeXmacs> sont pr\<#E9\>fix\<#E9\>s par <verbatim|tm->. Par exemple, le style
  <verbatim|<rigid|tm-plain>> de <TeXmacs> remplace le style
  <verbatim|plain> de <BibTeX>. Des \<#E9\>quivalents des styles <BibTeX>
  suivants ont \<#E9\>t\<#E9\> impl\<#E9\>ment\<#E9\>s : <verbatim|abbrv>, <verbatim|abstract>,
  <verbatim|acm>, <verbatim|alpha>, <verbatim|elsart-num>,
  <verbatim|ieeetr>, <verbatim|plain>, <verbatim|siam> et <verbatim|unsrt>.
  Ces styles peuvent donc \<#EA\>tre utilis\<#E9\>s sans installer <BibTeX>. Lorsque
  <BibTeX> n'est pas install\<#E9\> et qu'un document utilise un style <BibTeX>
  <verbatim|<em|nom>>, <TeXmacs> se rabat sur <verbatim|tm-<em|nom>>, ou sur
  <verbatim|tm-plain> si ce style n'existe pas. (Sans l'outil de bases de
  donn\<#E9\>es, ce remplacement est une liste fixe dans
  <source-link|edit_process.cpp|src/Edit/Process/edit_process.cpp>, qui ne
  contient pas <verbatim|abstract> : un document de style
  <verbatim|abstract> utilise alors <verbatim|tm-plain>.)

  L'utilisateur peut d\<#E9\>finir de nouveaux styles bibliographiques. Chaque
  style correspond \<#E0\> un unique fichier <scheme>, plac\<#E9\> dans le r\<#E9\>pertoire
  <verbatim|$TEXMACS_PATH/progs/bibtex>. Les fichiers de style sont des
  programmes <scheme> ordinaires. Comme l'\<#E9\>criture d'un style \<#E0\> partir de
  rien est une t\<#E2\>che complexe, nous vous conseillons d'adapter des fichiers
  de style ou des modules existants. Dans les sections suivantes, nous
  d\<#E9\>crivons la cr\<#E9\>ation d'un nouveau style sur un exemple simple, puis les
  fonctions <scheme> qui facilitent l'\<#E9\>criture de styles.

  La bibliographie est engendr\<#E9\>e par <cpp|generate_bibliography> dans
  <source-link|edit_process.cpp|src/Edit/Process/edit_process.cpp>, de deux
  fa\<#E7\>ons, selon la pr\<#E9\>f\<#E9\>rence <verbatim|"database tool">
  (<menu|Tools|Database tool>, d\<#E9\>sactiv\<#E9\>e par d\<#E9\>faut) :

  <\itemize>
    <item>Sans l'outil de bases de donn\<#E9\>es, les entr\<#E9\>es sont lues dans le
    fichier <verbatim|.bib> de la bibliographie et dans les r\<#E9\>f\<#E9\>rences de
    <TeXmacs> lui-m\<#EA\>me (<verbatim|$TEXMACS_PATH/misc/bib/texmacs.bib>). Pour
    un style <verbatim|tm-<em|nom>>, le module <verbatim|(bibtex <em|nom>)>
    est charg\<#E9\> et sa fonction <scm|bib-process> met en forme les entr\<#E9\>es ;
    les autres styles sont confi\<#E9\>s au programme <verbatim|bibtex>. En
    l'absence de fichier <verbatim|.bib>, un fichier <verbatim|.bbl> de m\<#EA\>me
    nom est utilis\<#E9\>.

    <item>Avec l'outil de bases de donn\<#E9\>es, <scm|bib-compile> dans
    <source-link|database/bib-manage.scm|TeXmacs/progs/database/bib-manage.scm>
    cherche chaque cl\<#E9\> cit\<#E9\>e dans les sources suivantes, dans cet ordre
    (voir <scm|bib-sources>) : les entr\<#E9\>es du document lui-m\<#EA\>me (les pi\<#E8\>ces
    jointes dont le nom se termine par <verbatim|-biblio>), le fichier
    <verbatim|.bib> de la bibliographie et les r\<#E9\>f\<#E9\>rences de <TeXmacs>, la
    base bibliographique de l'utilisateur (<scm|(bib-database)>), les
    fichiers <verbatim|.bib> tenus \<#E0\> jour par <name|Zotero>
    (<scm|zotero-managed-file?>), <name|Zotero> lui-m\<#EA\>me, et enfin les
    entr\<#E9\>es conserv\<#E9\>es dans le document (les pi\<#E8\>ces jointes dont le nom se
    termine par <verbatim|-bibliography>). Pour les styles de
    <scm|(bib-standard-styles)>, les entr\<#E9\>es trouv\<#E9\>es sont mises en forme
    par le style <scheme> ; pour les autres styles, les fichiers
    <verbatim|.bib> et les autres entr\<#E9\>es, converties en <BibTeX>, sont
    r\<#E9\>unis dans <verbatim|$TEXMACS_HOME_PATH/system/bib/auto.bib>, qui est
    confi\<#E9\> au programme <verbatim|bibtex>. Ensuite, <scm|bib-attach>
    conserve les entr\<#E9\>es trouv\<#E9\>es dans la pi\<#E8\>ce jointe
    <verbatim|<em|pr\<#E9\>fixe>-bibliography> du document, qui peut ainsi \<#EA\>tre
    compil\<#E9\> sans la base de donn\<#E9\>es. La fonction <scm|(bib-retrieve-entries
    noms . sources)> retrouve les entr\<#E9\>es d'une liste de cl\<#E9\>s dans une telle
    liste de sources.
  </itemize>

  Les fonctions de mise en forme des styles sont d\<#E9\>crites ci-dessous. Voir
  <hlink|la base de donn\<#E9\>es de <TeXmacs> et les
  bibliographies|../../source/database-bibliography.en.tm>,
  <hlink|Zotero|../../source/zotero.en.tm> et <hlink|les styles
  bibliographiques|../../source/bibtex-styles.en.tm> pour les m\<#E9\>canismes
  sous-jacents.

  <section|Exemple de style bibliographique simple>

  Les fichiers de style bibliographique sont plac\<#E9\>s dans le r\<#E9\>pertoire
  <verbatim|$TEXMACS_PATH/progs/bibtex>. Leur nom est celui du style, suivi
  de l'extension <verbatim|.scm>. Par exemple, <verbatim|example.scm> est le
  fichier du style <verbatim|example>, d\<#E9\>sign\<#E9\> par <verbatim|tm-example>
  dans un document <TeXmacs>.

  Tout fichier de style doit \<#EA\>tre d\<#E9\>clar\<#E9\> comme un module de la fa\<#E7\>on
  suivante :

  <\scm-code>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils) (bibtex plain)))
  </scm-code>

  Le module <verbatim|bib-utils> contient toutes les fonctions utiles pour
  \<#E9\>crire des styles bibliographiques ; le module du style par d\<#E9\>faut (ici
  <verbatim|plain>, voir ci-dessous) doit aussi \<#EA\>tre utilis\<#E9\>, afin que ses
  fonctions de mise en forme soient d\<#E9\>finies.

  Tout fichier de style doit \<#EA\>tre d\<#E9\>clar\<#E9\> comme un style bibliographique de
  la fa\<#E7\>on suivante :

  <scm-code|(bib-define-style "example" "plain")>

  Le premier argument de <scm|bib-define-style> est le nom du style
  courant, le second celui d'un style par d\<#E9\>faut, ici <verbatim|plain>. Si
  une fonction n'est pas d\<#E9\>finie dans le style courant, celle du style par
  d\<#E9\>faut est utilis\<#E9\>e. Ainsi, le fichier de style minimal suivant se
  comporte exactement comme le style <verbatim|plain> :

  <\scm-code>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils) (bibtex plain)))

    \;

    (bib-define-style "example" "plain")
  </scm-code>

  Chaque fonction de mise en forme du style par d\<#E9\>faut peut \<#EA\>tre red\<#E9\>finie
  dans le style courant. Par exemple, la fonction <scm|bib-format-date> met
  en forme la date dans le style <verbatim|plain>. On peut la red\<#E9\>finir dans
  notre style ainsi :

  <\scm-code>
    (tm-define (bib-format-date e)

    \ \ (:mode bib-example?)

    \ \ (bib-format-field e "year"))
  </scm-code>

  Toutes les fonctions export\<#E9\>es doivent \<#EA\>tre pr\<#E9\>fix\<#E9\>es par
  <verbatim|bib->. Les fonctions red\<#E9\>finies doivent \<#EA\>tre suivies de la
  directive <scm|(:mode bib-example?)>, o\<#F9\> <verbatim|example> est le nom du
  style courant.

  Notre fichier complet <verbatim|example.scm> est le suivant :

  <\scm-code>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils) (bibtex plain)))

    \;

    (bib-define-style "example" "plain")

    \;

    (tm-define (bib-format-date e)

    \ \ (:mode bib-example?)

    \ \ (bib-format-field e "year"))
  </scm-code>

  Il se comporte comme le style <verbatim|plain>, sauf que les dates sont
  mises en forme par notre fonction.

  Avec l'outil de bases de donn\<#E9\>es, <TeXmacs> n'utilise ses styles internes
  que pour les noms renvoy\<#E9\>s par <scm|(bib-standard-styles)> ; les autres
  noms de style sont confi\<#E9\>s \<#E0\> <BibTeX>. (Sans l'outil de bases de donn\<#E9\>es,
  tout style <verbatim|tm-<em|nom>> est charg\<#E9\> directement.) Pour rendre un
  nouveau style disponible dans les deux cas, il faut donc aussi \<#E9\>tendre
  cette liste, par exemple dans le fichier de style lui-m\<#EA\>me ou dans
  <verbatim|my-init-texmacs.scm> :

  <\scm-code>
    (tm-define (bib-standard-styles)

    \ \ (append (former) (list "tm-example")))
  </scm-code>

  Le module <verbatim|(bibtex example)> est charg\<#E9\> automatiquement
  lorsqu'une bibliographie de style <verbatim|tm-example> est engendr\<#E9\>e.

  <section|Fonctions <scheme> pour \<#E9\>crire des styles bibliographiques>

  <subsection|Gestion des styles>

  <\explain>
    <scm|(bib-define-style name default)><explain-synopsis|d\<#E9\>claration d'un
    style>
  <|explain>
    Cette fonction d\<#E9\>clare un style nomm\<#E9\> <scm|name> (une cha\<#EE\>ne) dont le
    style par d\<#E9\>faut est <scm|default> (une cha\<#EE\>ne). Le style est choisi en
    s\<#E9\>lectionnant <verbatim|tm-><scm|name> lors de l'ajout d'une
    bibliographie \<#E0\> un document. Lorsqu'une fonction de mise en forme n'est
    pas d\<#E9\>finie dans le style courant, sa d\<#E9\>finition dans le style par
    d\<#E9\>faut est utilis\<#E9\>e. Cette macro d\<#E9\>finit un mode <TeXmacs> dont le
    pr\<#E9\>dicat <scm|bib-<scm-arg|name>?> peut \<#EA\>tre utilis\<#E9\> dans les clauses
    <scm|(:mode ...)> de <scm|tm-define>.
  </explain>

  <\explain>
    <scm|(bib-with-style style fun . args)><explain-synopsis|style local>
  <|explain>
    Cette fonction applique <scm|fun> aux arguments <scm|args> comme si le
    style courant \<#E9\>tait <scm|style> (une cha\<#EE\>ne), et renvoie le r\<#E9\>sultat.
  </explain>

  <subsection|Fonctions sur les champs>

  <\explain>
    <scm|(bib-field entry field)><explain-synopsis|contenu d'un champ>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> correspondant au champ
    <scm|field> (une cha\<#EE\>ne) de l'entr\<#E9\>e <scm|entry>, sans mise en forme.
    Dans certains cas, le r\<#E9\>sultat est particulier :

    <\itemize-dot>
      <item>si <scm|field> vaut <scm|"author"> ou <scm|"editor">, le
      r\<#E9\>sultat est un arbre d'\<#E9\>tiquette <verbatim|bib-names> suivie d'une
      liste de noms d'auteurs ; chaque nom est un arbre d'\<#E9\>tiquette
      <verbatim|bib-name> \<#E0\> quatre \<#E9\>l\<#E9\>ments : le pr\<#E9\>nom, la particule (von),
      le nom et le suffixe (jr) ;

      <item>si <scm|field> vaut <scm|"pages">, le r\<#E9\>sultat est un arbre
      d'\<#E9\>tiquette <verbatim|bib-pages>, suivie d'un ou deux num\<#E9\>ros de page
      (des cha\<#EE\>nes), pour une page seule ou un intervalle de pages. Si le
      champ n'a pas pu \<#EA\>tre analys\<#E9\>, le num\<#E9\>ro de page est <scm|"0">.
    </itemize-dot>
  </explain>

  <\explain>
    <scm|(bib-format-field entry field)><explain-synopsis|mise en forme
    simple>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> correspondant au champ
    <scm|field> (une cha\<#EE\>ne) de l'entr\<#E9\>e <scm|entry>, avec une mise en
    forme simple : la premi\<#E8\>re lettre est mise en majuscule, sauf dans les
    blocs <verbatim|keep-case>. La cha\<#EE\>ne vide est renvoy\<#E9\>e si le champ est
    vide ou absent. Les variantes <scm|bib-format-field-preserve-case> et
    <scm|bib-format-field-locase-first> conservent la casse, <abbr|resp.>
    mettent la premi\<#E8\>re lettre en minuscule.
  </explain>

  <\explain>
    <scm|(bib-format-field-Locase entry field)><explain-synopsis|mise en
    forme particuli\<#E8\>re>
  <|explain>
    Cette fonction est semblable \<#E0\> <scm|bib-format-field>, mais le champ
    est mis en minuscules, avec une majuscule au d\<#E9\>but.
  </explain>

  <\explain>
    <scm|(bib-empty? entry field)><explain-synopsis|champ vide>
  <|explain>
    Cette fonction renvoie <scm|#t> si le champ <scm|field> (une cha\<#EE\>ne) de
    l'entr\<#E9\>e <scm|entry> est vide ou absent, et <scm|#f> sinon.
  </explain>

  <subsection|Fonctions de structuration>

  <\explain>
    <scm|(bib-new-block tm)><explain-synopsis|nouveau bloc>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> form\<#E9\> d'un bloc contenant l'arbre
    <scm|tm>, avec une majuscule au d\<#E9\>but et un point \<#E0\> la fin. La variante
    <scm|bib-new-case-preserved-block> ne change pas la casse.
  </explain>

  <\explain>
    <scm|(bib-new-list sep ltm)><explain-synopsis|liste avec s\<#E9\>parateur>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> form\<#E9\> de la concat\<#E9\>nation des
    \<#E9\>l\<#E9\>ments de la liste <scm|ltm>, s\<#E9\>par\<#E9\>s par l'arbre <scm|sep>.
  </explain>

  <\explain>
    <scm|(bib-new-list-spc ltm)><explain-synopsis|liste s\<#E9\>par\<#E9\>e par des
    espaces>
  <|explain>
    Cette fonction \<#E9\>quivaut \<#E0\> <scm|(bib-new-list " " ltm)>.
  </explain>

  <\explain>
    <scm|(bib-new-sentence ltm)><explain-synopsis|nouvelle phrase>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> correspondant \<#E0\> une phrase
    form\<#E9\>e des \<#E9\>l\<#E9\>ments de la liste <scm|ltm> s\<#E9\>par\<#E9\>s par des virgules. Les
    \<#E9\>l\<#E9\>ments vides sont omis. La variante
    <scm|bib-new-case-preserved-sentence> ne met pas la premi\<#E8\>re lettre en
    majuscule.
  </explain>

  <subsection|Fonctions de manipulation du texte>

  <\explain>
    <scm|(bib-abbreviate name dot spc)><explain-synopsis|abr\<#E9\>viation d'un
    nom>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> correspondant \<#E0\> l'abr\<#E9\>viation du
    nom contenu dans l'arbre <scm|name> : la liste des premi\<#E8\>res lettres de
    chaque mot, suivies de <scm|dot> (un arbre) et s\<#E9\>par\<#E9\>es par <scm|spc>
    (un arbre).
  </explain>

  <\explain>
    <scm|(bib-add-period tm)><explain-synopsis|point final>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> en ajoutant un point \<#E0\> la fin de
    <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-default-preserve-case tm)>

    <scm|(bib-default-upcase-first tm)><explain-synopsis|arbre <TeXmacs> par
    d\<#E9\>faut>
  <|explain>
    Ces fonctions simplifient l'arbre <scm|tm> ; la seconde met aussi sa
    premi\<#E8\>re lettre en majuscule (sauf dans les blocs <verbatim|keep-case>).
    Elles sont utilis\<#E9\>es par les fonctions de la famille
    <scm|bib-format-field>.
  </explain>

  <\explain>
    <scm|(bib-emphasize tm)><explain-synopsis|italique>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> correspondant \<#E0\> <scm|tm> en
    italique.
  </explain>

  <\explain>
    <scm|(bib-locase tm)><explain-synopsis|minuscules>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> \<#E9\>gal \<#E0\> <scm|tm> avec toutes les
    lettres en minuscules, sauf dans les blocs <verbatim|keep-case>.
  </explain>

  <\explain>
    <scm|(bib-prefix tm nbcar)><explain-synopsis|d\<#E9\>but d'un arbre
    <TeXmacs>>
  <|explain>
    Cette fonction renvoie une cha\<#EE\>ne form\<#E9\>e des <scm|nbcar> premiers
    caract\<#E8\>res de <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-upcase tm)><explain-synopsis|majuscules>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> \<#E9\>gal \<#E0\> <scm|tm> avec toutes les
    lettres en majuscules, sauf dans les blocs <verbatim|keep-case>.
  </explain>

  <\explain>
    <scm|(bib-upcase-first tm)><explain-synopsis|premi\<#E8\>re lettre en
    majuscule>
  <|explain>
    Cette fonction cr\<#E9\>e un arbre <TeXmacs> \<#E9\>gal \<#E0\> <scm|tm> avec sa premi\<#E8\>re
    lettre en majuscule, sauf dans les blocs <verbatim|keep-case>. De m\<#EA\>me,
    <scm|(bib-locase-first tm)> met la premi\<#E8\>re lettre en minuscule.
  </explain>

  <subsection|Fonctions diverses>

  <\explain>
    <scm|(bib-null? v)><explain-synopsis|valeur vide>
  <|explain>
    Cette fonction renvoie <scm|#t> si la valeur <scm|v> est vide (la
    cha\<#EE\>ne vide, la liste vide, ou un <markup|with> dont le corps est vide),
    et <scm|#f> sinon.
  </explain>

  <\explain>
    <scm|(bib-purify tm)><explain-synopsis|aplatissement d'un arbre
    <TeXmacs>>
  <|explain>
    Cette fonction renvoie une cha\<#EE\>ne form\<#E9\>e de toutes les lettres de
    l'arbre <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-simplify tm)><explain-synopsis|simplification d'un arbre
    <TeXmacs>>
  <|explain>
    Cette fonction renvoie un arbre <TeXmacs> correspondant \<#E0\> la
    simplification de l'arbre <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-text-length tm)><explain-synopsis|longueur d'un arbre
    <TeXmacs>>
  <|explain>
    Cette fonction renvoie la longueur de l'arbre <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-translate msg)><explain-synopsis|traduction>
  <|explain>
    Cette fonction renvoie le balisage <scm|(localize msg)>, qui traduit le
    message <scm|msg> (une cha\<#EE\>ne) de l'anglais vers la langue du document.
  </explain>

  <\explain>
    <scm|(bib-standard-styles)><explain-synopsis|styles internes>
  <|explain>
    Cette fonction renvoie la liste des noms (avec le pr\<#E9\>fixe
    <verbatim|tm->) des styles mis en oeuvre en <scheme>.
  </explain>

  <tmdoc-copyright|2010\U2026|l'\<#E9\>quipe <TeXmacs>>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>
