\ Ocaml syntax:
let foo x y ->
	(foo x y)
	sx <- x + 1
	<- y + 1

\ Old syntax:
word do-times <nq-*> ( n q -- ) [[
	$f <- $i <-

	$i ->> 0= if [[
		$f >-
		drop ret
	]]

	until [[
		sw: $i [[ 1- ]]
		$f ->> do
		$i ->> 0=
	]]

	$i >- $f >-
]]

\ New syntax:
(word do-times [[ num quote -- * ]] =>
    $f<- $i<-

    $i->> 0= (if $f>- drop ret)

    (until
        $i{ 1- }
        $f->> do
        $i->> 0=
    )

    $i>- $f>-
)

\ Same thing, but with auto-params and cleanup:
(word do-times [[ i:num q:quote -- * ]] =>
    $i->> 0= (if ret)

    (until
        $i{ 1- }
        $q->> do
        $i->> 0=
    )
)

\ Old syntax:
word .s <s- > [[
	0 until [[
		dup rot> & emit
		# rot 1+ tuck =
	]]
]]

\ New syntax, with params/cleanup:
\ (and with a new loop construct)
(word print [[ s:str -- ]] =>
	0 $i<-
	[$i->> $s->> # =] (untilq
		drop
		$i->> $s->> & emit
		$i{ 1+ }
	)
	$i>>-
)
