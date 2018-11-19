module Parser = 
	struct		
		type block = A | B | Text of string
    type result = EOF | Block of block
	end


