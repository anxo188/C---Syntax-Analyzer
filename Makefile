cminus2:	cminus2.tab.c cminus2.lex.c
	gcc -o cminus2 cminus2.tab.c lex.yy.c -lm
cminus2.tab.c:	cminus2.y
	bison -dv cminus2.y
cminus2.lex.c:	cminus2.l
	flex cminus2.l
clean:
	rm  cminus2.tab.c cminus2.tab.h cminus2.output lex.yy.c cminus2
