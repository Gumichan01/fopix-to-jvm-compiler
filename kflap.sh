if [ -z "$1" ] ; then
	./flap --verbose -s fopix -t kontix examples/javix.fx
else
	./flap --verbose -s fopix -t kontix $1
fi
