
all: Main.hs
	ghc --make Main.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f Main
	rm -f fvdrjeug.zip

zip: README.txt Main.hs functions.while fibonacci.while
	find *.hs $^ | zip fvdrjeug.zip -@

Main.hs: WhileParser.hs AnnotatedSyntaxTree.hs PrettyPrinter.hs
WhileParser.hs: ParserM.hs AbstractSyntaxTree.hs
AnnotatedSyntaxTree.hs: AbstractSyntaxTree.hs VariableMap.hs Evaluator.hs
PrettyPrinter.hs: AbstractSyntaxTree.hs AnnotatedSyntaxTree.hs VariableMap.hs
AbstractSyntaxTree.hs:
VariableMap.hs: AbstractSyntaxTree.hs
Evaluator.hs: AbstractSyntaxTree.hs VariableMap.hs
