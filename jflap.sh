if [ -z "$1" ] ; then
	./flap --verbose -s fopix -t javix -r true examples/javix.fx
else
	./flap --verbose -s fopix -t javix -r true $1
fi
