<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Écrire, sauvegarder, imprimer et charger un texte simple>

  <paragraph*|La saisie>

  Une fois que vous avez donné un nom à votre document, vous pouvez
  commencer à taper. Tapez simplement votre texte comme sur une machine à
  écrire : <TeXmacs> coupe automatiquement les lignes lorsqu'elles deviennent
  trop longues, et les justifie, c'est-à-dire qu'il ajuste les espaces entre
  les mots afin que toutes les lignes aient la même largeur. Il vous suffit
  d'appuyer sur <key|return> pour commencer un nouveau paragraphe. Le texte
  suivant est un exercice simple :

  <\big-figure>
    <\big-envbox>
      Hey you there!

      Would you be so kind to enroll the gnus and gnats around you to try
      <TeXmacs> once?<text-cursor>
    </big-envbox>
  <|big-figure>
    Un texte simple tapé avec <TeXmacs> (<verbatim|examples/test.tm>).
  </big-figure>

  Si vous faites une erreur, <key*|backspace> efface le caractère qui
  précède le curseur, et <menu|Edit|Undo> (<shortcut|(undo 0)>) annule vos
  dernières modifications. Si votre texte contient des caractères qui ne
  figurent pas sur votre clavier, comme des lettres accentuées, vous pouvez
  les taper avec les méthodes de votre système ; nous verrons plus loin
  comment taper des symboles mathématiques.

  <paragraph*|La sauvegarde>

  Lorsque vous avez fini de taper, sauvegardez votre document avec
  <menu|File|Save>, avec l'entrée <menu|Save> du menu de l'icône
  <icon|tm_save.png>, ou avec <shortcut|(save-buffer)>. Un message dans la
  barre d'état confirme que le document a été sauvegardé.

  Par sécurité, <TeXmacs> <em|sauvegarde automatiquement> votre document
  toutes les deux minutes, dans un fichier de sauvegarde séparé. Si vous
  oubliez de sauvegarder votre document avant de fermer <TeXmacs>, ou si
  votre ordinateur s'arrête pour une raison quelconque, <TeXmacs> vous
  proposera de récupérer les modifications non sauvegardées la prochaine
  fois que vous ouvrirez le document.

  <paragraph*|L'impression et l'exportation>

  Vous voudrez en général imprimer votre document, ou l'envoyer à des
  personnes qui n'utilisent pas <TeXmacs>. Vous imprimez votre document avec
  <menu|File|Print|Print buffer>, en utilisant les imprimantes de votre
  système. Vous pouvez aussi le transformer en fichier PDF, lisible sur
  n'importe quel ordinateur, avec <menu|File|Export|Pdf>. Le format du
  papier (A4 en Europe, Letter aux États-Unis) et les marges se règlent dans
  <menu|Document|Page>.

  <paragraph*|Le chargement>

  Pour retrouver votre texte après avoir fermé <TeXmacs>, utilisez
  <menu|File|Load>, l'entrée <menu|Load> du menu de l'icône
  <icon|tm_open.png>, ou le raccourci <shortcut|(open-document)>. Un
  sélecteur de fichiers apparaît, dans lequel vous sélectionnez le fichier
  <verbatim|test.tm> avant de confirmer, ou bien vous double-cliquez
  simplement dessus. Vos documents récents sont aussi listés dans
  <menu|File|Recent>.

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
