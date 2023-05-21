	.text
	################################################################
	# testTree function:
	# - parameter:
	#   - $a0: address of the root of the tree (or 0, if an empty tree)
	# - results:
	#   - $v0: 1 if the tree is a valid binary search tree; 0 if not
	#   - $v1:
	#     - -1 if not doing the AVL tree enhancement
	#     - 0 if it does not have the shape of a valid AVL tree
	#     - 1 if has the shape of a valid AVL tree
	################################################################

testTree:

	li $v0,1
	beqz $a0,return	#null tree is valid


	#$a0 should have address of root of tree
	#$a1 should have minimum 32-bit number
	#$a2 should have maximum 32-bit number
	li $a1, 0x80000000
	li $a2, 0x7FFFFFFF

	#saving return address
	subu $sp,4
	sw $ra,($sp)
	
	jal helpTestTree
	
	lw $ra, ($sp)
	addu $sp,4

return:
	li $v1,-1 #not doing AVL enhancement
	jr $ra


	################################################################
	# helpTestTree function:
	# - parameter:
	#   - $a0: address of the node (or 0 if null)
	#   - $a1: minimum value
	#   - $a2: maximum value
	# - results:
	#   - $v0: 1 if valid, 0 if not
	################################################################

helpTestTree:
	#if node is null that is valid
	beqz $a0,skipToLeft
	lw $t0,($a0)
	
	#node's val shouldn't be less than or equal to the minimum
	ble $t0,$a1,return0

	#node's val shouldn't be greater than or equal to the maximum
	bge $t0,$a2,return0


	#the left and right subtree also need to be valid (recursion)

skipToLeft:	
	#run on left subtree

	#making sure it isn't null before we check it
	lw $t1,4($a0)
	beqz $t1,skipToRight
	
	#saving registers
	subu $sp,16
	sw $ra,($sp)
	sw $a0,4($sp)
	sw $a1,8($sp)
	sw $a2,12($sp)

	lw $a2,($a0)	#saving what was the node to max
	lw $a0,4($a0)

	jal helpTestTree

	#load back arguments and $ra
	lw $ra,($sp)
	lw $a0,4($sp)
	lw $a1,8($sp)
	lw $a2,12($sp)
	addu $sp,16

	#checking to make sure $v0 didn't get set to zero during functional call above
	bnez $v0,skipToRight
	jr $ra


skipToRight:	
	#run on right subtree

	#making sure it isn't null before we check it
	lw $t1,8($a0)
	bnez $t1,keepGoing
	jr $ra

keepGoing:
	
	#saving registers
	subu $sp,16
	sw $ra,($sp)
	sw $a0,4($sp)
	sw $a1,8($sp)
	sw $a2,12($sp)

	lw $a1,($a0) 	#saving what was the node to min
	lw $a0,8($a0)

	jal helpTestTree

	#load back arguments and $ra
	lw $ra,($sp)
	lw $a0,4($sp)
	lw $a1,8($sp)
	lw $a2,12($sp)
	addu $sp,16

	jr $ra

return0:
	li $v0,0
	jr $ra



	################################################################
	# main entry point:
	# - prompts user for tree data
	# - prints a separator line
	# - prints the tree
	# - calls the testTree function
	# - prints the results of the testTree function
	# - exits the program
	################################################################
main:
	
	# print the prompt
	.data
prompt:	.ascii "Please type a sequence of integers for the tree,\n"
	.ascii "separated by whitespace.\n"
	.asciiz "End with non-integer (for example, 'X').\n"
	.text
	la $a0,prompt # address of string
	li $v0,4 # code for print string
	syscall

	# read the tree
	jal readTree
	move $s0,$v0 # save root-pointer in $s0

	# print the separator line
	.data
dashes:	.asciiz "=================================\n"
	.text
	la $a0,dashes # the string of '=' signs
	li $v0,4 # code for print string
	syscall

	# print the tree
	move $a0,$s0 # root pointer
	jal printTree

	# call the testTree function
	move $a0,$s0
	jal testTree

	# print whether a valid BST
	la $a0,validBst
	bnez $v0,readyBstReport
	la $a0,invalidBst
readyBstReport:	
	li $v0,4
	syscall
	
	.data
validBst:
	.asciiz "Tree is a valid BST\n"
invalidBst:
	.asciiz "Tree is not a valid BST\n"

	.text
	# print enhancement result
	la $a0,noEnhance
	bltz $v1,readyAvlReport
	la $a0,notAvl
	beqz $v1,readyAvlReport
	la $a0,isAvl
readyAvlReport:	
	li $v0,4
	syscall
	
	.data
noEnhance:
	.asciiz "AVL enhancement not done\n"
notAvl:
	.asciiz "Tree does not have an AVL shape\n"
isAvl:
	.asciiz "Tree has an AVL shape\n"

	.text
	# exit the program
	li $v0,10 # code for exit program
	syscall
	
	################################################################	
	# printTree: prints a tree ("sideways", using ASCII)
	# parameters:
	#   - $a0 = root of the tree to print
	################################################################	
printTree:
	li $a1,0 # set indentation level to 0
	li $a2,'=' # set direction character to '='
	
	# we now drop through to 'helpPrintTree'

	################################################################	
	# helpPrintTree: prints a tree
	# parameters:
	#   - $a0 = root of the tree to print
	#   - $a1 = indentation level
	#   - $a2 = direction character
	################################################################	
helpPrintTree:
	# if root is null, go exit immediately
	beq $a0,$zero,donePrintTree
	
	# prolog: save registers
	subu $sp,16
	sw $ra,0($sp)
	sw $s0,4($sp)
	sw $s1,8($sp)
	sw $s2,12($sp)
	
	# save tree node, indentation level and direction character
	move $s0,$a0
	move $s1,$a1
	move $s2,$a2
	
	# print right subtree
	li $a2,'/' # set direction character
	lw $a0,8($s0) # get right subnode
	addu $a1,$s1,1 # bumped indentation level
	jal helpPrintTree # recursively print right subtree
	
	# print blanks for indentation
	beq $s1,$zero,endPrintBlanksLoop # skip if no indentation
	move $t0,$s1 # for counting down indentations
printBlanksLoop:
	la $a0,twoBlanks # load address of string with three blanks
	.data
twoBlanks:
	.asciiz "  "
	.text
	li $v0,4 # code for print string
	syscall
	subu $t0,1 # decrement counter
	bne $t0,$zero,printBlanksLoop # if more to print, loop back
endPrintBlanksLoop:
	
	# print direction character
	move $a0,$s2 # character to print
	li $v0,11 # code for print character
	syscall

	# print our node's data value
	lw $a0,0($s0) # data value from node
	li $v0,1 # code for print integer
	syscall

	# print newline
	li $a0,'\n' # character to print: newline
	li $v0,11 # code for print character
	syscall
	
	# print left subtree
	li $a2,'\\' # set direction character
	lw $a0,4($s0) # get left subnode
	addu $a1,$s1,1 # bumped indentation level
	jal helpPrintTree # recursively print left subtree
	
	# epilog, restore registers
	lw $ra,0($sp)
	lw $s0,4($sp)
	lw $s1,8($sp)
	lw $s2,12($sp)
	addu $sp,16
	
	# return
donePrintTree:
	jr $ra

	################################################################	
	# readTree: reads a binary tree from standard input
	#
	# The values are expected to be integers, separated by whitespace,
	# listed in preorder. This means that the first element is the root
	# value; this is followed by all the values from the left subtree (also
	# in preorder); finally, the values in the right subtree are listed
	# in preorder. Termination is indicated by a non-integer character,
	# such as a letter or punctuation symbol.
	#
	# Note: if the list of integers is a valid preorder list, then a
	# legal binary search tree will be produced. If it not a valid
	# order list, a binary tree will be produced, but where some of the
	# elements are out of order
	#
	# For example, the input:
	#   100
	#   50
	#   25
	#   35
	#   80
	#   120
	#   130
	#   X
	# would produce the tree:
	#          100
	#         /   \
	#        50    120
	#       /  \     \
	#     25    80    130
	#       \
	#        35
	#   	
	# The input:
	#    X
	# would produce an empty tree.
	#
	# If the list of numbers to not correspond to a valid preorder
	# listing of a tree, the resulting tree will be out of order in
	# the binary search tree sense.
	#
	# parameters:
	#   - (none)
	# returns:
	#   - $v0 = address of the root of the tree node (or 0=null if empty)
	################################################################	
readTree:
	# save return address
	subu $sp,4
	sw $ra,($sp)
	
	# read the first integer
	jal readInteger
	move $a0,$v0 # left subtree's new data
	li $v0,0 # return value for the empty-tree case
	beqz $v1,returnReadTree
	
	# set the left-tree-limit to be the largest 32-bit integer
	li $a1,0x7fffffff
	
	# read the tree
	jal helpReadTree

	# restore return address
returnReadTree:	
	lw $ra,($sp)
	addu $sp,4

	# mask off low bit, which might have been a "done" flag
	and $v0,0xfffffffe
	
	jr $ra

	################################################################
	# helpReadTree: reads a binary search tree from standard input
	# assuming that the first integer has already been read
	#
	# parameters:
	# - $a0 = the number that has been most recently read from standard
	#   input
	# - $a1 = the smallest number that is too large for this subtree.
	#   This would indicate that we are in some left subtree, and that
	#   the number belongs in some right subtree of an ancestor.
	# returns:
	# - $v0 = a pointer to the node that is the root of the tree (or null),
	#   possibly with a 1 ORed into the low bit. If the low bit is 1,
	#   it indicates that no more numbers should be read.
	# - $v1 = the number that has been most recently read from standard
	#   input
	################################################################
helpReadTree:
		
	# if we're at or beyond the limit, just return an empty tree in $v0,
	# passing back the most recently read number in $v1
	bgt $a1,$a0,notNullHrt
	li $v0,0 # return value = null
	move $v1,$a0 # most recently read value
	jr $ra

notNullHrt:	
	# save registers
	subu $sp,20
	sw $ra,($sp)
	sw $s0,4($sp)
	sw $s1,8($sp)
	sw $s2,12($sp)
	sw $s3,16($sp)
	
	#####
	# note: the $s-registers are used as follows:
	# - $s0: most recent integer read by caller
	# - $s1: the limit for integers in the current subtree
	# - $s2: pointer to the node we're currently building
	# - $s3: the integer that we just read
	#####

	# save 'most recent read' and 'limit' and arguments in $s0 and $s1
	move $s0,$a0
	move $s1,$a1

	# read the next integer, to see if it's equal to the one passed
	# to us
	jal readInteger
	move $s3,$v0 # the data we just read

	# if we had a non-number, then create a node with the most
	# recently read integer, but with a 1 ORed into the pointer,
	# indicating "no more reading of values"
	bnez $v1,normal # skip if we have a value

	# allocate 16 bytes of memory for the tree node
	li $a0,12
	jal malloc
	sw $s0,($v0)
	sw $zero,4($v0)
	sw $zero,8($v0)
	or $v0,1
	move $v1,$s3 # return value we read as most recently read
	b finish # go finish up

normal:	
	# if we get here, we have a value that needs to be put into the
	# tree (in $s0)

	# allocate 16 bytes of memory for the tree node
	li $a0,12
	jal malloc
	
	# populate the data and the "aux" word
	move $s2,$v0 # save pointer in a register
	sw $s0,($v0) # store data value into node's data slot

	# read the left subtree
	move $a0,$s3 # value we read is now the "most recent data"
	move $a1,$s0 # our data is left subtree's limit
	jal helpReadTree
	
	# store pointer in left-subtree slot of our node
	and $t0,$v0,0xfffffffe # mask off "done" bit
	sw $t0,4($s2) # store pointer into node
	
	# if we have the "done" flag, store null in the right and return
        # with a 1 ORed into the pointer bit
	and $t0,$v0,0x1
	beq $t0,$zero,noFlagBit # test low bit
	sw $zero,8($s2) # store zero in right subnode
	or $v0,$s2,1 # or our "done" bit into the node address
	b finish # go return

noFlagBit:	
	# if we get here, then we actually need to read more data values
	# to create the right subtree

	# create/read the right subtree
	move $a0,$v1 # most recently read value
	move $a1,$s1 # our limit is also the right subtree's limit
	jal helpReadTree
	
	# store pointer in right-subtree slot of our node
	and $t0,$v0,0xfffffffe # mask off "done" bit
	sw $t0,8($s2) # store pointer into node
	
	# OR flag bit given from reading right subtree into our $v0
	# return value. ($v1 already has the correct value.)
	and $v0,$v0,1
	or $v0,$s2 # address of our root, with possible '1' in low bit

	# restore registers and return
finish:	
	lw $ra,($sp)
	lw $s0,4($sp)
	lw $s1,8($sp)
	lw $s2,12($sp)
	lw $s3,16($sp)
	addu $sp,20

	# return
	jr $ra
	
	################################################################
	# readInteger - reads an integer from the keyboard
	# returns:
	# - $v0 - the integer read, if any
	# - $v1 - the return status
	#   - 0 => an integer was not read--typically because a character was
	#     encountered that is not legal in an integer
	#   - 1 => a value integer was read
	# 
	# This function skips over whitespace. It is expected that the
	# the integer will be followed by whitespace.
	#
	# This function does not check for overflow, so the if the number
	# typed in is too large, the result will only be the bottom
	# 32 bits of the true result.
	#
	################################################################
readInteger:	
	
	# prolog
	subu $sp,8
	sw $ra,($sp)
	sw $s0,4($sp)

	# skip any whitespace found
skipWhite:	
	li $v0, 12 # read character
	syscall
	move $a0,$v0
	jal isWhitespace
	bnez $v0,skipWhite
	move $t2,$v1

	# registers will be used as follows:
	# - $t0 = -1 if a '-' sign was seen, 1 otherwise
	# - $t1 = the accumlated value as we are reading digits
	# - $v2 = the most recent character read

	# if the current character is a '-', gobble it up, and mark
	# the result as being negative.
	li $t3,'-'
	li $t0,1
	bne $t2,$t3,doneWithMinus
	li $t0,-1
	li $v0,12
	syscall
	move $t2,$v0
	
doneWithMinus:	
	
	# test the first character for being a digit, return 0 (failure)
	# if not
	li $v1,0 # tentative return value
	li $t1,0 # start with 0 in building number
	li $t3,'0'
	blt $t2,$t3,finishRi
	li $t3,'9'
	bgt $t2,$t3,finishRi

	# We have a first digit in $t2. Enter loop to process digits.

readLoop:	
	# subtract the ASCII offset for '0' from the character, and add
	# it into the accumulator
	subu $t2,'0'
	addu $t1,$t2

	# read the next character, test for being digit
	li $v0,12
	syscall
	move $t2,$v0 # the character read
	li $v1,1 # tentative return value: status
	move $v0,$t1 # tentative return value: integer
	li $t3,'0'
	blt $t2,$t3,negateVal
	li $t3,'9'
	bgt $t2,$t3,negateVal
	
	# multiply current value by 10, in anticipation of addition of
	# value for the digit we just read.
	mul $t1,$t1,10

	# loop back
	b readLoop

negateVal:
	# multiply by negation factor
	mul $s0,$t1,$t0 # negate, if needed

	# we have our number. Make sure that the termination character
	# is whitespace
	move $a0,$t2
	jal isWhitespace
	move $v1,$v0 # whether terminated legally
	move $v0,$s0 # our number

finishRi:	
	# epilog
	lw $ra,($sp)
	lw $s0,4($sp)
	addu $sp,8

	# return
	jr $ra

	################################################################
	# isWhitespace - tells whether a character represents whitespace
	# parameter:
	# - $a0: the character value. It is expected that the top three
	#   bytes are zero
	# returns:
	# - $v0 = 1 if the character is whitespace, 0 otherwise
	# - $v1 = the character value tested
	#
	# Whitespace is considered to be codes 9 (tab), 10 (newline),
	# 13 (formfeed) and 32 (space).
	################################################################
isWhitespace:	

	# set tentative return values
	li $v0,1
	move $v1,$a0
	
	# if its one of our whitespace characters, to return
	li $t0,' '
	beq $a0,$t0,doneIws
	li $t0,9
	beq $a0,$t0,doneIws
	li $t0,10
	beq $a0,$t0,doneIws
	li $t0,13
	beq $a0,$t0,doneIws

	# not a whitespace character: put a 0 into the return-value
	li $v0,0
	
doneIws:
	# return
	jr $ra

	################################################################
	# malloc: allocates memory from the heap. Always allocates an
	# address that is a multiple of 4.
	#
	# parameters:
	# - $a0 = the number of bytes needed
	# returns:
	# - $v0 = a pointer to the allocated memory
	#
	# WARNINGS:
	# - the memory is not guaranteed to be initialized to zero
	# - there is no way to return memory to the system
	# - no checking is done that there is enough memory available. If
	#   not, illegal addresses will be allocated. These might end up
	#   being address of non-existent memory, addresses on the stack,
	#   etc.
	################################################################
malloc:	
	# return value
	lw $v0,heapFreePointer
	
	# round up to multiple of 4, because we want to be word-aligned
	addu $a0,3 
	and $a0,0xfffffffc
	addu $a0,$v0
	
	# store new free-pointer back
	sw $a0,heapFreePointer

	# return
	jr $ra
	
	################################################################
	# memory area for the heap
	################################################################
	
	.data
	.align 2
	# pointer to lowest unused heap address
heapFreePointer:
	.word heapStart
	
	# heap memory
heapStart:
	.space 100000
