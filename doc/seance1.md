Notes de la séance 1 de Compil M2
=================================

### Objectif du cours/projet de cette année ###

 - langage d'entrée: *Fopix*.

 - langage cible: la *JVM* fournie par Java

 - projet à réaliser par groupe de deux au plus


### Visite guidée de Fopix ###

 - l'AST et ses constructions

 - la syntaxe concrète : voir exemples, ou les PrettyPrint et Parser fournis

 - caractéristiques principales :
   - des fonctions récursives, pas d'application partielle
   - un poil de fonctionnel : la tête d'une application est à calculer (cf. `FunName`)
   - dans un premier temps vous pourrez vous concentrer sur le cas simple des `(FunCall(FunName _,_))`
   - des tableaux impératifs. Primitives : `New` a 1 argument (taille), `Get` en a 2 (tab,idx), `Set` en a 3 (tab,idx,val)

### La JVM ###

 - présentation des principaux éléments de la Java Virtual Machine:
   - un frame par appel de fonction, avec une pile et une zone de variables locales
   - un tas général (utilisé ici lors d'un `anewarray`)
 - description de quelques instructions, cf par exemple la [spec de la JVM](http://docs.oracle.com/javase/specs/jvms/se8/html/index.html)
 - demo du programme assembleur [jasmin](http://jasmin.sourceforge.net/) pour aller d'un fichier `.j` à un `.class`
 - voir aussi le désassembleur `javap -c`
 - un interprète fourni, cf JavixInterpreter

### La première séance de TP ###

 - Prise en main du compilateur `flap`, de ses options
 - Compléter FopixInterpreter (facultatif)
 - Prise en main de jasmin et/ou du pseudo-langage Javix de notre compilateur
