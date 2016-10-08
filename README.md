# Mode d'emploi

Présentation
==

Tyrme et le nom d'une petite machine virtuelle developpé en OCaml


La compilation
==

make tyrme

make clean

make cleanall


L'execution du programme
==

./tyrme [-c/-i/-e/-dinstr] [-d] fichier.[ty/bty]

Les option -c, -i, -e, -dinstr sont obligatoire, il faudra toujours les renseigner.
 
L'option -d est elle optionnelle. Le fichier devra être d'extansion .ty ou .bty selon qu'il s'agisse de code source ou de byte-code. 

L'option -c compile le fichier fichier.ty et génère un fichier fichier.bty contenant le byte-code. 

L'option -i interprete du code source, c'est à dire quelle va éxécuter le fichier fichier.ty sans avoir besoin de le compiler en code octet. 

L'option -e executera le byte-code du fichier fichier.bty. 

L'option -dinstr affichichera la liste d'instruction d'un fichier .ty ou .bty. Et enfin, l'option -d sera un debuggueur, il affichera chaque étape de l'éxécution de la machine virtuelle.


