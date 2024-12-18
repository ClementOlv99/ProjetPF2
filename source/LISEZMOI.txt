Le code est decoupe en 2 repertoires :
- lib/ contient les modules et interfaces implementant les divers elements du jeu.s n'implemente pas le programme principale. Le fichier lib/dune contient une directive qui fait que dune creera une bibliotheque appelle libnewtonoid contenant tous ces modules de lib/ et utilisable par les modules de bin/. Ces modules peuvent utiliser les bibliotheque "graphics" et "unix" ainsi que les tests de la forme "let%test".
- bin/ contient les modules du programme principal (notamment, le module principal : newtonoid.ml). Ces modules peuvent utiliser les modules de la bibliotheque libnewtonoid (en faisant un "open Libnewtonoid"). Le fichier bin/dune ne permet pas, dans la configuration qui vous est donnee, que les modules de bin/ utilisent d'autres bibliotheques que Libnewtonoid (comme "graphics" et "unix", ou les tests "let%test").

Le repertoire rapport/ contiendra votre rapport au format PDF.
