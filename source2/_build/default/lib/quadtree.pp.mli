Caml1999N031����            0lib/quadtree.mli����  E  W  }  �����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,inline_tests�@�@@����'enabled��.<command-line>A@A�A@H@@��A@@�A@I@@@@�@@����������������,library-name�@�@@����,libnewtonoid��A@A�A@M@@��A@N@@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@�����A�    �(quadtree��0lib/quadtree.mliC R W�C R _@@@@A@���)ocaml.docА������	,
	le type du QuadTree à proprement parler.
��D ` `�F � �@@@@@@@��C R R@@�@���A�    �%coord��H � ��H � �@@@@A��������%float��)H � ��*H � �@@�@@@�����%float��3H � ��4H � �@@�@@@@�@@@���4�������	Q
	pour stocker les coordonnées de différents objets: coord = (float * float) .
��CI � ��DK@@@@@@@��FH � ��GH � �@@�@���A�    �'limites��QL�RL@@@@A��������%coord��]L�^L@@�@@@�����%coord��gL�hL"@@�@@@@�@@@���h7�������43@3@@@3@3���rA�������	�
	pour delimiter un espace, on stock le coin inferieur gauche (noté dans le code "sud-est") et le coin superieur droit (noté dans le code "nord-ouest"): limites = (coord * coord) .
���M##��O��@@@@@@@���L@@�@���Р'predict���Q����Q��@��@����%float���Q����Q��@@�@@@��@����������%float���Q����Q��@@�@@@�����%float���Q���Q�@@�@@@@�@@@��������%float���Q���Q�@@�@@@�����%float���Q���Q�@@�@@@@�@@@@���Q����Q�@@@����$list���Q�%��Q�)@�����%coord���Q���Q�$@@�@@@@�@@@���Q��@@@�P@@@@������������
  B
	fonction de calcul des points de la prédiction de la balle.
	- signature : predict : float -> ((float * float) * (float * float)) -> coord list

	- paramètre(s) :
		- le rayon de la balle.
		- (les coordonnées de la balle * les vitesses de la balle).

	- résultat :
		- les trois points de test de colisions future.
���R**��\oq@@@@@@@���Q��@� @���Р+create_tree���^sw� ^s�@��@����'limites��	^s��
^s�@@�@@@����(quadtree��^s��^s�@@�@@@�@@@@���␠�����	�
	fonction de création du QuadTree initial.
	- signature : create_tree : coord list -> tree

	- paramètre(s) :
		- les limites de l'écran.
		- la liste des briques.

	- résultat :
		- un QuadTree avec la position de chacunes des briques.
��"_���#i��@@@@@@@��%^ss@�@���Р+insert_tree��.k���/k��@��@����(quadtree��8k���9k��@@�@@@��@����%coord��Ck���Dk��@@�@@@����(quadtree��Lk���Mk��@@�@@@�@@@�@@@@���N�������
  ,
	fonction pour rajouter une brique dans un QuadTree en respectant la structure du QuadTree et ses invariants.
	- insert_tree : tree -> coord -> tree

	- paramètre(s) :
		- le QuadTree incomplet.
		- les coordonnées de la brique à rajouter.

	- résultat :
		- le QuadTree avec la brique en plus.
��]l���^v��@@@@@@@��`k��@�@���Р)find_tree��ix���jx�@��@����(quadtree��sx��tx�@@�@@@��@����%coord��~x��x�@@�@@@����&option���x� ��x�&@�����%coord���x���x�@@�@@@@�@@@�@@@�#@@@@����b�������
  Z
	fonction de recherche de brique à portée.

	- signature : find_tree : tree -> coord -> coord option

	- paramètre(s) :
		- le QuadTree representant le niveau.
		- un couple de float representant la position de la balle.

	- résultat :
		- None si pas de briques à portée.
		- Some (coord) les coordonnées de la brique incriminée sinon.
���y''�� E��@@@@@@@���x��@�@���Р,find_briques��� G���� G��@��@����(quadtree��� G���� G��@@�@@@��@�������%coord��� G���� G��@@�@@@��������%float��� G���� G��@@�@@@�����%float��� G���� G��@@�@@@@�@@@@��� G��@@@����$list��� G���� G��@�����%coord��� G���� G��@@�@@@@�@@@��� G��@@@�A@@@@����Ő������
  �
	fonction de recherche des briques à portées d'une balle (à priori les briques les plus proches des trois points de la projection de la balle.)

	- find_briques : quadtree -> coord list -> coord list

	- paramètre(s) :
		- le QuadTree representant le niveau.
		- un coord representant la position de la balle et un couple de float representant la vitesse de la balle.

	- résultat :
		- Une liste des briques potentiellement à portées.
�� H��� S	�	�@@@@@@@�� G��@� @���Р*purge_tree�� U	�	�� U	�	�@��@����(quadtree�� U	�	�� U	�	�@@�@@@��@����$list��& U	�	��' U	�	�@�����%coord��/ U	�	��0 U	�	�@@�@@@@�@@@����(quadtree��9 U	�	��: U	�	�@@�@@@�@@@�#@@@@���;
�������
  2
	fonction de retrait d'une liste de briques d'un QuadTree.

	- signature : purge_tree : quadtree -> coord -> quadtree

	- paramètre(s) :
		- le QuadTree representant le niveau.
		- une liste de couple d'entiers representant les positions des briques à retirer.

	- résultat :
		- le QuadTree corrigé.
��J V	�	��K a
�
�@@@@@@@��M U	�	�@�@���Р,draw_briques��V c
��W c
�@��@����(quadtree��` c
��a c
�@@�@@@��@����#int��k c
��l c
�!@@�@@@��@����#int��v c
�%�w c
�(@@�@@@����$unit�� c
�,�� c
�0@@�@@@�@@@�@@@�%@@@@@��� c
�
�@�@@