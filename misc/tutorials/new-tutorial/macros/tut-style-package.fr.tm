<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Réutiliser vos macros dans d'autres documents>

  Les macros du préambule n'existent que dans leur document. Si vous voulez
  les utiliser aussi dans d'autres documents, placez-les dans un
  <em|paquetage de style> : un fichier portant le suffixe <verbatim|.ts>, qui
  contient des définitions de macros et que vous pouvez ajouter à n'importe
  quel document.

  <paragraph*|Créer le paquetage>

  Ouvrez le document contenant vos macros et choisissez
  <menu|Tools|Macros|Extract style package>. <TeXmacs> ouvre un nouveau
  document dans le style <tmstyle|source>, qui contient un en-tête (avec le
  nom de paquetage <verbatim|love-macros>, que vous pouvez changer en
  <verbatim|love>), les paramètres de style de votre document s'il y en a,
  et toutes les définitions du préambule :

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>

    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  Sauvegardez ce document dans le dossier <verbatim|packages> de votre
  dossier personnel <TeXmacs>, sous le nom <verbatim|love.ts>. Ce dossier
  personnel est <verbatim|~/.TeXmacs> sous <name|Linux> et <name|macOS>
  (c'est-à-dire le dossier <verbatim|.TeXmacs> de votre dossier
  d'utilisateur), et <verbatim|%APPDATA%\\TeXmacs> sous <name|Windows> ;
  <TeXmacs> crée le dossier <verbatim|packages> à l'intérieur au démarrage.
  Sous <name|macOS>, le sélecteur de fichiers masque les dossiers dont le
  nom commence par un point : appuyez sur <verbatim|Cmd-Shift-.> pour
  afficher <verbatim|.TeXmacs>. Le paquetage d'exemple de ce tutoriel est
  <verbatim|examples/love.ts>.

  <paragraph*|Utiliser le paquetage>

  Dans n'importe quel document, vous pouvez maintenant choisir
  <menu|Document|Style|Add package|Add other package> et taper
  <verbatim|love> : les macros <markup|hi> et <markup|love> deviennent
  disponibles, exactement comme si elles avaient été définies dans le
  préambule du document. Si vous modifiez le paquetage par la suite, tous
  les documents qui l'utilisent tiennent compte de la modification. On peut
  retirer un paquetage d'un document dans le menu <menu|Document|Style>.

  Vous pouvez aussi créer un paquetage de style à partir de rien, en
  choisissant <menu|Document|Style|source> pour un nouveau document, en
  tapant les définitions et en le sauvegardant avec le suffixe
  <verbatim|.ts>. L'écriture de fichiers de style est expliquée en détail
  dans <menu|Help|Manual|Writing your own style files>.

  <tmdoc-copyright|1998\U2026|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
