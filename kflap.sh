if [ -z "$1" ] ; then
	./flap --verbose -s fopix -t jakix -r true examples/javix.fx
else
	./flap --verbose -s fopix -t kontix $1
fi
