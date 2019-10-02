#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.
Smalltalk at: #queue put: SortedCollection new.

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]

   writeBitArray: bitArray [
   		bitArray do: [:i|
   			self writeBit: i
   		].
   		self flushByte.		
   ]
]


Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [ ^ true]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count) | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: outbits [
   		outbits writeBit: 0.
   		char = 0 ifTrue: 
   					[[8 to: 1 by: -1 do: [outbits writeBit: 0]].
   					outbits writeBit: 0.].
   		char = 256 
   				ifTrue: [[8 to: 1 by: -1 do: [outbits writeBit:0]].
   						outbits writeBit: 1.]
   				ifFalse: [
   					| ascii currBit |
   					ascii := char asCharacter.
   					8 to: 1 by: -1 do: [:i|
   						currBit := ascii bitAt: i.
   						outbits writeBit: currBit.
   					].S
   				].
   		]
  ]

Leaf subclass: Tree [
   |left right|

   isLeaf [ ^ false]

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: outbits [
   		left postorder: outbits.
   		right postorder: outbits.
   		outbits writeBit: 1.
   ]


]


optC := false.
optU := false.
optT := false.
optD := false.

freqtable := ZeroArray new: 257.
1 to: 256 do: [:i|
	freqtable at: i put: 0
].



Smalltalk arguments: '-c|--compress: -u|--uncompress: -t|--tree: -d|--debug' 
		  do: [:opt :arg|
	
		  	(opt = $c) ifTrue: [optC := true].
		 
		  	(opt = $u) ifTrue: [optU := true].

		  	(opt = $t) ifTrue: [optT := true].		 

		  	(opt = $d) ifTrue: [optD := true].


		  	(arg isNil) ifFalse: [
		  		inputfilename := Smalltalk getArgv: 2.
		  		outputfilename := Smalltalk getArgv: 3.
		  		
		  		inputfile := FileStream open: inputfilename mode: FileStream read.
		  		(outputfilename isNil)
		  			ifTrue: [outputfile := stdout]
		  			ifFalse: [outputfile := FileStream open: outputfilename mode: FileStream write].
		  	



		  	"Print encode table"		
			optT ifTrue: [
				| encodetable |
				(inputfile atEnd not) ifTrue: [
					| currChar |
					currChar := inputfile next asInteger.
					freqtable incr: currChar.
				].

				freqtable at: 256 put: 1.

				freqtable keysAndValuesDo: [:char :count |
					| node |
					((char isNil not) and: (count > 0)) ifTrue: [
						node := Leaf new: (char asCharacter) count: count.
						queue add: node.
					].
				].

				[queue size > 1] whileTrue: [
					| leftnode rightnode newtree|
					leftnode := queue removeFirst.
					rightnode := queue removeFirst.
					newtree := Tree new: (leftnode char) count: ((leftnode count) + (rightnode count))
									left: leftnode right: rightnode.
					queue add: newtree.
				].



				encodetable := ZeroArray new:257.
				tree := queue first.

				tree inorder: [:char :string |
					encodetable at: char put: string
				] prefix: ''.

				1 to: 256 do: [:i|
					outputfile << i visible

				].
			]. 
	

			"Compression option"
			optC ifTrue: [
				| encodetable |
				(inputfile atEnd not) ifTrue: [
					| currChar |
					currChar := inputfile next asInteger.
					freqtable at: currChar put: ((freqtable at: currChar) incr).
				].

				freqtable at: 256 put: 1.

				freqtable keysAndValuesDo: [:k :v |
					| node |
					((k isNil not) and: (v > 0)) ifTrue: [
						node := Leaf new: (k asCharacter) count: v.
						queue add: node.
					].
				].

				[queue size > 1] whileTrue: [
					| leftnode rightnode newtree|
					leftnode := queue removeFirst.
					rightnode := queue removeFirst.
					newtree := Tree new: (leftnode char) count: ((leftnode count) + (rightnode count))
									left: leftnode right: rightnode.
					queue add: newtree.
				].

				encodetable := ZeroArray new:257.
				tree := queue removeFirst.

				tree inorder: [:char :string |
					encodetable at: char asInteger put: string.
				] prefix: ''.

				outBytes := OutBits new: outputfile.
				tree postorder: outBytes.
				outBytes writeBit: 1.

			].
		].

		optU ifTrue: [



		]

] ifError: [
	stderr << 'Error: Invalid usage' << nl.
	ObjectMemory quit: 1.
].



