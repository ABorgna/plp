Object subclass: #Direction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Direction commentStamp: '<historical>' prior: 0!
Direction!


!Direction methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:52'!
nextFrom: puntoDeOrigen
"Dado un punto, devuelve el resultado de moverse en la direccion correspondiente. "
^self class nextFrom: puntoDeOrigen ! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Direction class
	instanceVariableNames: ''!

!Direction class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:54'!
nextFrom: puntoDeOrigen
	^ self subclassResponsibility ! !


!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
southwest
	^Southwest new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:53'!
north
	^North new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:52'!
south
	^South new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
northwest
	^Northwest new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:53'!
east
	^East new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
northeast
	^Northeast new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:53'!
west
	^West new! !

!Direction class methodsFor: 'instance-creation' stamp: 'PLP 10/9/2017 17:59'!
southeast
	^Southeast new! !


Direction subclass: #East
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!East commentStamp: '<historical>' prior: 0!
East!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

East class
	instanceVariableNames: ''!

!East class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:32'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x + 1)@(puntoDeOrigen y)! !


Object subclass: #Flag
	instanceVariableNames: 'flagTeam'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Flag commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Flag methodsFor: 'setting' stamp: 'Anonymous 10/29/2017 19:25'!
team: aTeam 
	flagTeam := aTeam.! !


!Flag methodsFor: 'accesing' stamp: 'Anonymous 10/29/2017 19:28'!
team
	^flagTeam.! !

!Flag methodsFor: 'accesing' stamp: 'ab 11/28/2017 23:35'!
getIfVisible
	^ self! !

!Flag methodsFor: 'accesing' stamp: 'Anonymous 10/29/2017 18:53'!
isVisible
	^true.! !


Object subclass: #Game
	instanceVariableNames: 'terrain flags teams currentTeamIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Game commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Game methodsFor: 'initialization' stamp: 'ab 11/28/2017 23:07'!
initializeWithX: anInteger Y: anInteger2 trapLocations: trapLocations flagLocations: flagLocations
	terrain := (Terrain dimX: anInteger dimY: anInteger2) .
	self items: Trap at: trapLocations .
	flags := self items: Flag at: flagLocations .
	teams := OrderedCollection new .
	currentTeamIndex := 1 .! !


!Game methodsFor: 'as yet unclassified' stamp: 'ab 11/28/2017 22:57'!
currentWinners
	| teamFlags flagsPerTeam maxFlags teamNames |
	
	teamFlags := flags select: [ :f | f team isNotNil ] .
	teamFlags ifEmpty: [ ^ { } ] .
	
	flagsPerTeam  := teamFlags groupedBy: [ :f | f team ] .
	maxFlags := (flagsPerTeam collect: [ :fs | fs size ]) max .
	teamNames := teams collect: [ :t | t name ] .
	^ teamNames select: [ :tn | (flagsPerTeam at: tn ifAbsent: {}) size = maxFlags ]! !

!Game methodsFor: 'as yet unclassified' stamp: 'ab 11/28/2017 23:20'!
items: aClass at: positions
	^ (1 to: positions size) asOrderedCollection collect: [ :i |
		| obj |
		obj := aClass new .
		terrain at: (positions at: i) put: obj .
		obj
	]! !

!Game methodsFor: 'as yet unclassified' stamp: 'ab 11/8/2017 14:43'!
registerTeam: aTeamName playerLocations: aPlayerCollection 
	| players |
	players := aPlayerCollection pairsCollect: [ :pos :unitType  |
		unitType new: terrain with: pos and: aTeamName ] .
	teams add: (GameTeam new: aTeamName with: players) .! !

!Game methodsFor: 'as yet unclassified' stamp: 'ab 11/8/2017 15:06'!
moveCharacter: aMemberIndex to: aDirection steps: steps
	| unit |
	unit := self currentTeam members at: aMemberIndex .
	unit move: steps to: aDirection .
	self pass! !


!Game methodsFor: 'handling' stamp: 'ab 11/8/2017 14:56'!
pass
	currentTeamIndex := (currentTeamIndex % teams size) + 1! !


!Game methodsFor: 'accessing' stamp: 'ab 11/8/2017 14:48'!
currentTeam
	^ teams at: currentTeamIndex.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Game class
	instanceVariableNames: ''!

!Game class methodsFor: 'instance creation' stamp: 'gflan 11/23/2017 16:55'!
newX: anInteger Y: anInteger2 trapLocations: trapLocations flagLocations: flagLocations
	^ self new initializeWithX: anInteger Y: anInteger2 trapLocations: trapLocations flagLocations: flagLocations  
	! !


Object subclass: #GameTeam
	instanceVariableNames: 'name members'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!GameTeam commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!GameTeam methodsFor: 'initialization' stamp: 'gflan 11/23/2017 17:02'!
initialize: aString with: aCollection 
	name := aString .
	members := aCollection .
	! !


!GameTeam methodsFor: 'as yet unclassified' stamp: 'ab 11/7/2017 18:14'!
members
	^ members! !


!GameTeam methodsFor: 'accessing' stamp: 'ab 11/7/2017 18:16'!
name
	^ name! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

GameTeam class
	instanceVariableNames: ''!

!GameTeam class methodsFor: 'instance creation' stamp: 'gflan 11/23/2017 17:03'!
new: aString with: aCollection 
	^ self new initialize: aString with: aCollection ! !


Direction subclass: #North
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!North commentStamp: '<historical>' prior: 0!
North!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

North class
	instanceVariableNames: ''!

!North class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:33'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x)@(puntoDeOrigen y + 1)! !


Direction subclass: #Northeast
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Northeast commentStamp: '<historical>' prior: 0!
Northeast!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Northeast class
	instanceVariableNames: ''!

!Northeast class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:57'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x + 1)@(puntoDeOrigen y + 1)! !


Direction subclass: #Northwest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Northwest commentStamp: '<historical>' prior: 0!
Northwest!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Northwest class
	instanceVariableNames: ''!

!Northwest class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:57'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x - 1)@(puntoDeOrigen y + 1)! !


Direction subclass: #South
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!South commentStamp: '<historical>' prior: 0!
South!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

South class
	instanceVariableNames: ''!

!South class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:33'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x)@(puntoDeOrigen y - 1)! !


Direction subclass: #Southeast
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Southeast commentStamp: '<historical>' prior: 0!
Southeast!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Southeast class
	instanceVariableNames: ''!

!Southeast class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:58'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x + 1)@(puntoDeOrigen y - 1)! !


Direction subclass: #Southwest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Southwest commentStamp: '<historical>' prior: 0!
Southwest!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Southwest class
	instanceVariableNames: ''!

!Southwest class methodsFor: 'evaluating' stamp: 'PLP 10/9/2017 17:58'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x - 1)@(puntoDeOrigen y - 1)! !


Object subclass: #Terrain
	instanceVariableNames: 'matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Terrain commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Terrain methodsFor: 'as yet unclassified' stamp: 'ab 11/7/2017 17:17'!
dimX: anInteger dimY: anInteger2 
	matrix := Matrix rows: anInteger2 columns: anInteger.! !

!Terrain methodsFor: 'as yet unclassified' stamp: 'ab 11/7/2017 18:02'!
elementsAround: aUnit satisfying: aBlockClosure 
	^ (self elementsAround: aUnit) select: aBlockClosure .! !


!Terrain methodsFor: 'accessing' stamp: 'ab 11/7/2017 17:21'!
at: aPoint
	^ matrix at: aPoint y at: aPoint x ! !

!Terrain methodsFor: 'accessing' stamp: 'AutoDeprecationRefactoring 11/28/2017 23:36'!
visibleAt: aPoint
	^ (self at: aPoint) ifNotNil: [ :obj | ^ obj getIfVisible ]! !

!Terrain methodsFor: 'accessing' stamp: 'ab 11/7/2017 17:20'!
at: aPoint put: anObject
	matrix at: aPoint y at: aPoint x put: anObject ! !

!Terrain methodsFor: 'accessing' stamp: 'ab 11/30/2017 20:03'!
elementsAround: aUnit 
	| points |
	points := aUnit location eightNeighbors select: [:x | self contains: x] .
	points := points select: [ :p | (self at: p) isNotNil ] .
	^ aUnit selectSeenFrom: (points collect: [ :p | self at: p ]) .! !


!Terrain methodsFor: 'removing' stamp: 'gflan 11/23/2017 17:14'!
remove: anObject 
	matrix replaceAll: anObject  with: nil! !


!Terrain methodsFor: 'enumerating' stamp: 'ab 11/7/2017 17:30'!
select: aBlockClosure 
	^ (matrix asBag select: [:x | x isNotNil ]) select: aBlockClosure ! !


!Terrain methodsFor: 'as yet unclassified ' stamp: 'ab 11/7/2017 17:19'!
contains: aPoint 
	| inRangeX inRangeY |
	inRangeX := (aPoint x > 0) & (aPoint x <= matrix columnCount).
	inRangeY := (aPoint y > 0) & (aPoint y <= matrix rowCount) .
	^ inRangeX & inRangeY ! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Terrain class
	instanceVariableNames: ''!

!Terrain class methodsFor: 'as yet unclassified' stamp: 'ab 11/7/2017 17:07'!
dimX: anInteger dimY: anInteger2 
	| tmp |
	tmp := self new.
	tmp dimX: anInteger dimY: anInteger2.
	^tmp! !


TestCase subclass: #TestsEx1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!

!TestsEx1 methodsFor: 'testing' stamp: 'Anonymous 10/29/2017 19:17'!
test1_2_visibilityAndValue
	| flag trap |
	flag := Flag new.
	trap := Trap new.
	self assert: (flag isVisible).
	self assert: (trap isVisible not).
	self assert: (flag value) equals: flag.
	self assert: (trap value isNil).! !

!TestsEx1 methodsFor: 'testing' stamp: 'PLP 10/12/2017 11:54'!
test1_1_canCreateTrapsAndFlags
	self assert: (Flag new isKindOf: Flag).
	self assert: (Trap new isKindOf: Trap).! !

!TestsEx1 methodsFor: 'testing' stamp: 'Anonymous 10/29/2017 19:30'!
test1_3_flagTeams
	| flag1 flag2 flag3 |
	flag1 := Flag new.
	flag2 := Flag new team: #azul; yourself.
	flag3 := Flag new.
	
	self assert: flag1 team isNil.
	flag1 team: #azul.
	self assert: flag1 team isNil not.
	flag3 team: #rojo.
	self assert: flag1 team equals: flag2 team.
	self assert: flag3 team equals: #rojo.
	flag2 team: flag3 team.
	self assert: flag2 team equals: #rojo.
	! !


TestCase subclass: #TestsEx2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!

!TestsEx2 methodsFor: 'testing' stamp: 'PLP 10/13/2017 09:48'!
test2_4_remove
	"Verifica la presencia o ausencia de puntos en el terreno"

	| terrain flag |
	terrain := Terrain dimX: 5 dimY: 4.
	flag := Flag new.
	terrain at: (2@1) put: flag; at: (2@2) put: Flag new; at: (4@4) put: Flag new.
	self assert: (terrain at: (2@1)) isNotNil.
	
	terrain remove: flag.
	
	self assert: (terrain at: (2@1)) isNil.
	self assert: (terrain at: (2@2)) isNotNil.
	self assert: (terrain at: (2@4)) isNil.
	self assert: (terrain at: (4@4)) isNotNil.
	
	terrain remove: flag.
	self assert: (terrain at: (2@1)) isNil.
	self assert: (terrain at: (2@2)) isNotNil.
	self assert: (terrain at: (2@4)) isNil.
	self assert: (terrain at: (4@4)) isNotNil.
	! !

!TestsEx2 methodsFor: 'testing' stamp: 'PLP 10/12/2017 14:37'!
test2_1_pointsInTerrain
	"Verifica la presencia o ausencia de puntos en el terreno"

	| terrain |
	terrain := Terrain dimX: 5 dimY: 4.
	self assert: (terrain contains: (1@1)).
	self assert: (terrain contains: (5@2)).
	self assert: (terrain contains: (5@4)).
	self assert: (terrain contains: (4@5)) not.
	self assert: (terrain contains: (2@7)) not.
	self assert: (terrain contains: (0@1)) not.! !

!TestsEx2 methodsFor: 'testing' stamp: 'PLP 10/12/2017 14:48'!
test2_2_gameElementsInTerrain
	"Verifica la presencia o ausencia de puntos en el terreno"

	| terrain |
	terrain := Terrain dimX: 5 dimY: 4.
	terrain at: (2@1) put: Flag new; at: (3@2) put: Flag new; at: (4@4) put: Flag new.
	terrain at: (2@2) put: Trap new; at: (3@1) put: Trap new; at: (5@4) put: Trap new.
	self assert: ((terrain at: (2@3)) isNil).
	self assert: ((terrain at: (2@1)) isNotNil).
	self assert: ((terrain visibleAt: (2@1)) isNotNil).
	self assert: ((terrain at: (3@1)) isNotNil).
	self assert: ((terrain visibleAt: (3@1)) isNil).
	self assert: ((terrain at: (5@4)) isVisible not).
	self assert: ((terrain at: (4@4)) team isNil).! !

!TestsEx2 methodsFor: 'testing' stamp: 'PLP 10/12/2017 14:58'!
test2_3_select
	"Verifica la presencia o ausencia de puntos en el terreno"

	| terrain |
	terrain := Terrain dimX: 5 dimY: 4.
	terrain at: (2@1) put: Flag new; at: (3@2) put: Flag new; at: (4@4) put: Flag new.
	terrain at: (2@2) put: Trap new; at: (3@1) put: Trap new; at: (5@4) put: Trap new.
	self assert: ((terrain select: [:x| x isNotNil]) size) equals: 6.
	self assert: ((terrain select: [:x| x isVisible]) size) equals: 3.! !


TestCase subclass: #TestsEx3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!

!TestsEx3 methodsFor: 'testing' stamp: 'PLP 10/13/2017 09:16'!
test3_1_gameCharacterCreation
	| terrain bob dora doc |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (1@2) and: #verde.
	doc := Medic new: terrain with: (4@2) and: #verde.
	
	self assert: dora team equals: bob team.
	self assert: doc team equals: #verde.
	self assert: (terrain at: (1@2)) equals: bob.
	self should: [ Explorer new: terrain with: (2@5) and: #verde ] raise: Exception.
	self assert: dora canHeal not.
	self assert: bob canHeal not.
	self assert: doc canHeal.! !

!TestsEx3 methodsFor: 'testing' stamp: 'PLP 10/12/2017 15:50'!
test3_3_elementsAround
	"El mensaje elementsAround: debe devolver, dado un personaje, la colección de elementos adyacentes 
	al personaje que este puede ver. Recordar que solo los ingenieros pueden ver las trampas."
	| terrain bob dora doc |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (1@2) and: #verde.
	doc := Medic new: terrain with: (4@2) and: #verde.
	
	self assert: (terrain elementsAround: dora) size equals: 2.
	self assert: (terrain elementsAround: doc) size equals: 1.
	self assert: (terrain elementsAround: bob) size equals: 3.! !

!TestsEx3 methodsFor: 'testing' stamp: 'PLP 10/12/2017 16:00'!
test3_4_elementsAroundSatisfying
	| terrain bob dora doc |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (1@2) and: #verde.
	
	self assert: (terrain elementsAround: dora satisfying: [:x | x isVisible]) size equals: 2.
	self assert: (terrain elementsAround: bob satisfying: [:x | x isVisible]) size equals: 2.
	
	doc := Medic new: terrain with: (1@1) and: #verde.
	
	self assert: (terrain elementsAround: doc satisfying: [:x | x ~= 0]) size equals: 2.
	self assert: (terrain elementsAround: bob satisfying: [:x | x ~= #verde]) size equals: 4.
	self assert: (terrain elementsAround: bob satisfying: [:x | x respondsTo: #team]) size equals: 3.
	self assert: (terrain elementsAround: doc satisfying: [:x | x == bob]) size equals: 1.! !

!TestsEx3 methodsFor: 'testing' stamp: 'PLP 10/12/2017 15:29'!
test3_2_selectSeen
	"Esto testea el mensaje selectSeenFrom:, que dada una colección filtra los elementos que el receptor puede ver."
	| terrain bob dora doc col |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (1@2) and: #verde.
	doc := Medic new: terrain with: (4@2) and: #verde.
	
	col := OrderedCollection with: Flag new with: dora with: Trap new with: bob with: Trap new with: doc.
	
	self assert: (dora selectSeenFrom: col) size equals: 4.
	self assert: (bob selectSeenFrom: col) size equals: 6.
	self assert: (doc selectSeenFrom: col) size equals: 4.
	self assert: ((bob selectSeenFrom: col) anySatisfy: [:x | x isVisible not]).
	self assert: ((doc selectSeenFrom: col) allSatisfy: [:x | x isVisible]).! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TestsEx3 class
	instanceVariableNames: ''!

!TestsEx3 class methodsFor: 'initializing' stamp: 'PLP 10/12/2017 15:06'!
terrain
	| terrain |
	terrain := Terrain dimX: 5 dimY: 4.
	terrain at: (2@1) put: Flag new; at: (3@2) put: Flag new; at: (4@4) put: Flag new.
	terrain at: (2@2) put: Trap new; at: (3@1) put: Trap new; at: (5@4) put: Trap new.
	^terrain
	! !


TestCase subclass: #TestsEx4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!

!TestsEx4 methodsFor: 'testing' stamp: 'PLP 10/13/2017 09:37'!
test4_1_mobility
| terrain bob dora doc |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (2@1) and: #verde.
	doc := Medic new: terrain with: (3@4) and: #verde.
	
	self assert: bob canMove.
	self assert: doc canMove.
	self assert: dora canMove.
	
	dora disable.
	
	self assert: dora canMove not.
	self should: [ dora healedBy: bob ] raise: Exception.
	self assert: dora canMove not.
	self assert: doc canHeal.
	
	dora healedBy: doc.
	
	self assert: dora canMove.
	
	doc disable.
	
	self assert: doc canHeal not.
	self assert: doc canMove not.
	self should: [ doc healedBy: doc ] raise: Exception.! !

!TestsEx4 methodsFor: 'testing' stamp: 'PLP 10/13/2017 10:38'!
test4_5_interactWithAllElements
| terrain bob dora doc |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (3@2) and: #verde.
	doc := Medic new: terrain with: (3@4) and: #verde.
	
	dora disable.
	
	self assert: ((terrain at: (3@3)) isKindOf: Trap).
	self assert: ((terrain at: (4@2)) isKindOf: Trap).
	self assert: (terrain at: (2@2)) team isNil.
	self assert: dora canMove not.
	
	bob interactWithAllElements.
	doc interactWithAllElements.
	dora interactWithAllElements.
	
	self assert: (terrain at: (3@3)) isNil.
	self assert: (terrain at: (4@2)) isNil.
	self assert: (terrain at: (2@2)) team equals: #verde.
	self assert: dora canMove.
	! !

!TestsEx4 methodsFor: 'testing' stamp: 'PLP 10/15/2017 02:04'!
test4_6_moving
	"moveTo: mueve el personaje un paso en la dirección indicada, si es posible.
	move:to: mueve el personaje una cantidad dada de pasos en la dirección indicada, mientras sea posible."
	| terrain bob dora doc dean |
	terrain := self class terrain.
	dora := Explorer new: terrain with: 2 @ 3 and: #verde.
	dean := Explorer new: terrain with: 2 @ 4 and: #rojo.
	bob := Engineer new: terrain with: 2 @ 1 and: #verde.
	doc := Medic new: terrain with: 3 @ 4 and: #verde.
	
	self assert: ((terrain at: 3 @ 3) isKindOf: Trap).
	self assert: (dean canMoveTo: Direction southeast).
	
	dora moveTo: Direction east.
	
	self assert: dora location equals: 3 @ 3.
	self assert: (terrain at: 3 @ 3) equals: dora.
	self assert: dora canMove not.
	dora moveTo: Direction south.
	self assert: dora location equals: 3 @ 3.
	self assert: (dean canMoveTo: Direction southeast) not.
	"No se puede mover hacia el sudeste porque está Dora."
	
	dean moveTo: Direction southeast.
	
	self assert: dean location equals: 2 @ 4.
	
	doc moveTo: Direction east.
	
	self assert: dora canMove.	"Doc la curó al pasar a su lado."
		
	dean moveTo: Direction south.
	
	self assert: dean location equals: 2 @ 3.
	self assert: (terrain at: 4 @ 3) team isNil.
	self assert: (terrain at: 2 @ 2) team equals: #rojo.
	
	dora moveTo: Direction south.
	self assert: dora location equals: 3 @ 2.
	self assert: (terrain at: 4 @ 3) team equals: #verde.
	self assert: (terrain at: 2 @ 2) team equals: #verde.
	
	dean moveTo: Direction west.
	
	self assert: (terrain at: 4 @ 3) team equals: #verde.
	self assert: (terrain at: 2 @ 2) team equals: #rojo.
	self assert: ((terrain at: 4 @ 2) isKindOf: Trap).
	
	bob move: 3 to: Direction east.
	
	self assert: bob location equals: 4 @ 1.	"Se movió 2 lugares y se chocó con la bandera."
	self assert: ((terrain at: 4 @ 2) isNil).! !

!TestsEx4 methodsFor: 'testing' stamp: 'PLP 10/13/2017 09:51'!
test4_2_directions
| terrain bob dora doc |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (2@1) and: #verde.
	doc := Medic new: terrain with: (3@4) and: #verde.
	
	self assert: (bob canMoveTo: Direction northeast).
	self assert: (doc canMoveTo: Direction west).
	self assert: (dora canMoveTo: Direction east).
	self assert: (dora canMoveTo: Direction south) not.
	self assert: (dora canMoveTo: Direction northeast) not.
	self assert: (doc canMoveTo: Direction north) not.
	
	bob disable.
	
	self assert: (bob canMoveTo: Direction northeast) not.! !

!TestsEx4 methodsFor: 'testing' stamp: 'PLP 10/13/2017 10:30'!
test4_4_actions
| terrain bob dora doc flag trap |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	bob := Engineer new: terrain with: (2@1) and: #verde.
	doc := Medic new: terrain with: (3@4) and: #verde.
	flag := terrain at: (2@2).
	trap := terrain at: (1@2).
	
	dora disable.
	
	self assert: (trap isKindOf: Trap).
	self assert: flag team isNil.
	self assert: dora canMove not.
	
	bob action value: trap.
	doc action value: dora.
	dora action value: flag.
	
	self assert: (terrain at: (1@2)) isNil.
	self assert: flag team equals: #verde.
	self assert: dora canMove.
	! !

!TestsEx4 methodsFor: 'testing' stamp: 'PLP 10/15/2017 01:20'!
test4_3_targets
"Cada personaje tiene su objetivo. El mensaje target debe devolver un bloque que,
dado un objeto, indique si este es un objetivo para el personaje receptor."
| terrain bob dora doc dean |
	terrain := self class terrain.
	dora := Explorer new: terrain with: (2@3) and: #verde.
	dean := Explorer new: terrain with: (2@4) and: #rojo.
	bob := Engineer new: terrain with: (2@1) and: #verde.
	doc := Medic new: terrain with: (3@4) and: #verde.
	
	dora disable.
	dean disable.
	
	self assert: (dora target value: Flag new).
	self assert: (dora target value: Trap new) not.
	self assert: (doc target value: dora).
	self assert: (doc target value: bob) not.
	self assert: (doc target value: dean) not.
	self assert: (doc target value: Trap new) not.
	self assert: (bob target value: Trap new).
	self assert: (bob target value: Flag new) not.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TestsEx4 class
	instanceVariableNames: ''!

!TestsEx4 class methodsFor: 'initializing' stamp: 'PLP 10/13/2017 09:22'!
terrain
	| terrain |
	terrain := Terrain dimX: 5 dimY: 4.
	terrain at: (5@1) put: Flag new; at: (2@2) put: Flag new; at: (4@3) put: Flag new.
	terrain at: (1@2) put: Trap new; at: (4@2) put: Trap new; at: (3@3) put: Trap new.
	^terrain! !


TestCase subclass: #TestsEx5
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!

!TestsEx5 methodsFor: 'testing' stamp: 'PLP 10/15/2017 05:30'!
test5_1_gameTeam
	| terrain dora doc greenTeam|
	terrain := self class terrain.
	dora := Explorer new: terrain with: 2@3 and: #verde.
	doc := Medic new: terrain with: 3@4 and: #verde.	

	greenTeam := GameTeam new: #verde with: (OrderedCollection with: dora with: doc).

	self assert: greenTeam members size equals: 2.
	self assert: (greenTeam members includes: dora).
	self assert: greenTeam name equals: #verde.
! !

!TestsEx5 methodsFor: 'testing' stamp: 'PLP 10/15/2017 09:50'!
test5_3_game
	"Ya se puede empezar a jugar.
	El mensaje moveCharacter:to:steps: mueve el iésimo personaje del equipo actual una cierta cantidad 
	de pasos en la dirección indicada."
	| game trapLocations flagLocations |

	flagLocations := OrderedCollection with: (5@1) with: (2@2) with: (4@3).
	trapLocations := OrderedCollection with: (1@2) with: (4@2) with: (3@3).
	
	game := Game newX: 5 Y: 4 trapLocations: trapLocations flagLocations: flagLocations.

	game registerTeam: #verde playerLocations: {(2@3) . Explorer . (3@4) . Medic}.
	game registerTeam: #rojo  playerLocations: {(2@4) . Explorer . (2@1) . Engineer}.

	self assert: game currentTeam name equals: #verde.
	game pass.

	self assert: game currentTeam name equals: #rojo.
	game pass.

	self assert: game currentTeam name equals: #verde.
	game moveCharacter: 1 to:  Direction east steps: 1.
	self assert: game currentWinners isEmpty.

	self assert: game currentTeam name equals: #rojo.
	game moveCharacter: 1 to:  Direction south steps: 1.
	self assert: (game currentWinners includes: #rojo).
	
	self assert: game currentTeam name equals: #verde.
	game moveCharacter: 2 to:  Direction east steps: 1.
	self assert: (game currentWinners includes: #rojo).
	
	self assert: game currentTeam name equals: #rojo.
	game moveCharacter: 2 to:  Direction east steps: 3.
	self assert: (game currentWinners includes: #rojo).
	
	self assert: game currentTeam name equals: #verde.
	game moveCharacter: 1 to:  Direction south steps: 1.
	self assert: (game currentWinners includes: #rojo) not.
	self assert: (game currentWinners includes: #verde).

	self assert: game currentTeam name equals: #rojo.
	game moveCharacter: 1 to:  Direction west steps: 1.
	self assert: (game currentWinners includes: #rojo).
	self assert: (game currentWinners includes: #verde).
	! !

!TestsEx5 methodsFor: 'testing' stamp: 'ab 11/7/2017 18:28'!
test5_2_registerTeam
	| game trapLocations flagLocations playerLocations|

	flagLocations := OrderedCollection with: (5@1) with: (2@2) with: (4@3).
	trapLocations := OrderedCollection with: (1@2) with: (4@2) with: (3@3).
	
	game := Game newX: 5 Y: 4 trapLocations: trapLocations flagLocations: flagLocations.

	playerLocations := {(2@3). Explorer. (3@4). Medic}.
	
	game registerTeam: #verde playerLocations: playerLocations.

	self assert: game currentTeam members size equals: 2.
	self assert: game currentTeam name equals: #verde.
	self assert: (game currentTeam members first isKindOf: Explorer).
	self assert: ((game currentTeam members at: 2) isKindOf: Medic).
	self assert: (game currentTeam members at: 2) team equals: (game currentTeam name).


! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TestsEx5 class
	instanceVariableNames: ''!

!TestsEx5 class methodsFor: 'initializing' stamp: 'PLP 10/15/2017 02:32'!
terrain
	| terrain |
	terrain := Terrain dimX: 5 dimY: 4.
	terrain at: (5@1) put: Flag new; at: (2@2) put: Flag new; at: (4@3) put: Flag new.
	terrain at: (1@2) put: Trap new; at: (4@2) put: Trap new; at: (3@3) put: Trap new.
	^terrain! !


Object subclass: #Trap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Trap commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Trap methodsFor: 'accesing' stamp: 'ab 11/28/2017 23:36'!
getIfVisible
	^ nil! !

!Trap methodsFor: 'accesing' stamp: 'Anonymous 10/29/2017 19:13'!
value
	^nil.! !

!Trap methodsFor: 'accesing' stamp: 'Anonymous 10/29/2017 18:54'!
isVisible
	^false.! !


Object subclass: #Unit
	instanceVariableNames: 'isDisabled terrain location team'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Unit commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Unit methodsFor: 'as yet unclassified' stamp: 'ab 11/7/2017 17:40'!
team
	^ team! !

!Unit methodsFor: 'as yet unclassified' stamp: 'ab 11/28/2017 23:36'!
getIfVisible
	"A unit is always visible"
	^ self! !

!Unit methodsFor: 'as yet unclassified' stamp: 'ab 11/7/2017 17:45'!
selectSeenFrom: aCollection 
	^ aCollection select: [:x | x isVisible ] .! !

!Unit methodsFor: 'as yet unclassified' stamp: 'ab 11/7/2017 17:54'!
location
	^ location! !


!Unit methodsFor: 'initialization' stamp: 'ab 11/30/2017 19:14'!
initializeIn: aTerrain with: aPoint and: aString 
	team := aString .
	isDisabled := false .
	terrain := aTerrain .
	location := aPoint .
	aTerrain at: aPoint put: self.! !


!Unit methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 18:09'!
canHeal
	^ self subclassResponsibility! !

!Unit methodsFor: 'ej4' stamp: 'ab 11/30/2017 19:15'!
canMove
	^isDisabled not.! !

!Unit methodsFor: 'ej4' stamp: 'ab 11/30/2017 19:15'!
disable
	isDisabled := true.! !

!Unit methodsFor: 'ej4' stamp: 'ab 11/30/2017 19:16'!
healedBy: aUnit
	aUnit canHeal ifFalse: [Exception new signal: 'Solo medicos pueden curar unidades'].
	isDisabled := false . ! !

!Unit methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 20:32'!
move: anInteger to: aDirection
	anInteger timesRepeat: [ self moveTo: aDirection ] ! !

!Unit methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 19:57'!
action
	^ self subclassResponsibility! !

!Unit methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 17:58'!
moveTo: aDirection
	| content |
	(self canMoveTo: aDirection) ifFalse: [ ^self].
	terrain remove: self.
	location := aDirection nextFrom: location.
	content := terrain at: location.
	terrain at: location put: self.
	content ifNil: [self interactWithAllElements] ifNotNil: [ self disable ].
	^self! !

!Unit methodsFor: 'ej4' stamp: 'ab 11/28/2017 23:22'!
canMoveTo: aDirection 
	| nextLocation nextLocationIsValid |
	nextLocation := aDirection nextFrom: location. 
	nextLocationIsValid := [ (terrain contains: nextLocation) 
			and: [
				(terrain at: nextLocation) isNil 
				or: [ (terrain at: nextLocation) isVisible not ] 
			] 
		]. 
	^ self canMove and: nextLocationIsValid! !

!Unit methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 18:55'!
target
	^ self subclassResponsibility ! !

!Unit methodsFor: 'ej4' stamp: 'ab 11/30/2017 19:16'!
isDisabled
	^ isDisabled ! !

!Unit methodsFor: 'ej4' stamp: 'ab 11/30/2017 19:16'!
interactWithAllElements
	(terrain elementsAround: self satisfying: [:x | self target value:x ]) do: [ :item | self action value:item ]! !


!Unit methodsFor: 'testing' stamp: 'ab 11/7/2017 17:47'!
isVisible
	^ true.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Unit class
	instanceVariableNames: ''!

!Unit class methodsFor: 'instance creation' stamp: 'gflan 11/23/2017 17:07'!
new: aTerrain with: aPoint and: aString 
	^ self new initializeIn: aTerrain with: aPoint and: aString! !


Unit subclass: #Engineer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Engineer commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Engineer methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 19:14'!
target
	^ [ :x | x isKindOf: Trap ]! !

!Engineer methodsFor: 'ej4' stamp: 'ab 11/7/2017 17:49'!
selectSeenFrom: aCollection 
	^ aCollection .! !

!Engineer methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 17:48'!
canHeal
	^false! !

!Engineer methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 20:27'!
action
	^ [ :x | terrain remove:x ]! !


Unit subclass: #Explorer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Explorer commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Explorer methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 19:14'!
target
	^ [ :x | x isKindOf: Flag ]! !

!Explorer methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 17:48'!
canHeal
	^false! !

!Explorer methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 20:27'!
action
	^ [ :x | x team: team ]! !


Unit subclass: #Medic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!Medic commentStamp: '<historical>' prior: 0!
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points!


!Medic methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 20:22'!
target
	^ [ :x | (x isKindOf: Unit) and: [ x isDisabled and: [ x team = team ] ] ]! !

!Medic methodsFor: 'ej4' stamp: 'ab 11/30/2017 19:17'!
canHeal
	^isDisabled not ! !

!Medic methodsFor: 'ej4' stamp: 'Anonymous 11/3/2017 20:28'!
action
	^ [ :x | x healedBy: self ]! !


Direction subclass: #West
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PLP'!
!West commentStamp: '<historical>' prior: 0!
West!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

West class
	instanceVariableNames: ''!

!West class methodsFor: 'evaluating' stamp: 'PLP 10/14/2017 23:34'!
nextFrom: puntoDeOrigen

	^(puntoDeOrigen x - 1)@(puntoDeOrigen y)! !
