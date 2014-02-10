namespace BTree
module BTree = 
    open Microsoft.FSharp.Core

    type internal StructureNode<'a,'b> = Single of  'a*TreeNode<'a,'b> | Double of 'a*TreeNode<'a,'b>*TreeNode<'a,'b>
    and internal TreeNode<'a,'b> = Structure of StructureNode<'a,'b> list| Leaf of ('a*'b) list    
    type public BTree<'a,'b when 'a : equality> = int*TreeNode<'a,'b>

    //Create is easy, just new up a node
    let Create<'a,'b> k = (k,[])



    let rec private subFind target node   = 
        match node with
            |Leaf(l) -> l
            |Structure(l) -> List.fold (fun n sNode -> match sNode with
                                                        | Double(key,lnode,rnode) -> if target >= key then rnode else lnode
                                                        | Single(key,rnode) -> if target >= key then rnode else n)
                                        (Structure([])) //Meaningless, just to get us started
                                        l
                             |> fun x -> match x with
                                         | Leaf(l) -> l
                                         | Structure(s) -> subFind target (Structure(s))

    //If we're in a leaf try to locate the item in the leaf
    //Otherwise, use the above to find the leaf node and then look for the key
    let Find<'a,'b when 'a:comparison> ((k,tree):BTree<'a,'b>) key =
        let findInLeaf l = List.tryFind (fun (a,b) -> if a = key then true else false) l
        match tree with 
        | Leaf(l) -> findInLeaf l
        |  s -> findInLeaf (subFind key s)


    
    type private InsertResult<'a,'b> = Success of TreeNode<'a,'b> | Split of TreeNode<'a,'b>*'a*TreeNode<'a,'b>

    let Insert<'a,'b when 'a : comparison> (k,tree) (key:'a) (value:'b) = 
        //To insert into a leaf with space we partition into left and right elements
        let insertInOrder l = let (leftL,rightL) = List.partition (fun (a,b)-> a < key) l
                              //If we have this key already we update it's value otherwise add it
                              let newRight = match rightL with
                                             |((hkey,hvalue)::rest) -> if hkey = key then (key,value)::rest
                                                                       else (key,value)::(hkey,hvalue)::rest
                                             | _ -> (key,value)::rightL
                              (List.append leftL newRight)


        //Split a leaf in half
        let splitLeaf l = let left:('a*'b) list = List.ofSeq(Seq.take(k) l)
                          let right = List.ofSeq  (Seq.skip (k) l)
                          in
                          Split(Leaf(left),(fst (List.head right)),Leaf(right))
                         
                               
        //Here we insert the item into the leaf. If the resulting list is <= our branch factor
        //we're done, if not, we split the list
        let InsertLeaf l = let newL = (insertInOrder l)
                           in 
                           if List.length newL <= (2*k) then Success(Leaf(newL))
                           else splitLeaf newL
        
        //Note the weird list reversals are so we can avoid having to do the (O(n) insert into the
        //left list every time. We put the elements in backwards, then reverse them once at the end in O(n) time
        let rec partitionStructure left candidate right =
            match right with
            | [] -> (List.rev left,candidate,[])
            | r::rest -> let k = match r with
                                 |Single(skey,_) -> skey
                                 |Double(skey,_,_) -> skey
                         if key >= k then (partitionStructure (candidate::left) r rest)
                         else (List.rev left,candidate,r::rest)  
        
        

        //This is where the magic happens
        let rec insert t = 
            let insertStructure s =
                let (left,candidate,right) = partitionStructure [] (List.head s) (List.tail s)
                let result = match candidate with
                             | Single(sKey,n1) -> insert n1
                             | Double(sKey,n1,n2) -> if key < sKey then (insert n1) else (insert n2)
                match result with
                //If the node inserted successfully we return successful
                |Success(node) -> let newCandidate = match candidate with
                                                     |Single(sKey,n1) -> Single(sKey,node)
                                                     |Double(sKey,n1,n2) -> if key >= sKey then Double(sKey,n1,node) else Double(sKey,node,n2)
                                  Success(Structure(left @ [newCandidate] @ right))
                | Split(lSplit,newKey,rSplit) -> let newCandidates = match candidate with 
                                                                     | Single(sKey,n1) -> [Single(sKey,lSplit);Single(newKey,rSplit)]
                                                                     | Double(sKey,n1,n2) -> if key >= sKey then [Double(sKey,n1,lSplit);Single(newKey,rSplit)]
                                                                                             else [Double(newKey,lSplit,rSplit);Single(sKey,n2)]
                                                 let newList = left @ newCandidates @ right
                                                 if (List.length newList) < 2*k then Success(Structure(newList))
                                                 else
                                                    let newRight = List.ofSeq (Seq.skip k newList) 
                                                    let newKey = match List.head newRight with
                                                                 | Single(newkey,_) -> newKey
                                                                 | _ -> failwith "Invalid entry in the center of the list"
                                                    Split(Structure(List.ofSeq(Seq.take k newList)),newKey,Structure(newRight))

            match t with
            |Leaf(l) -> InsertLeaf l
            |Structure(s) ->   insertStructure(s)

            //We're at the root at this point, if we split, it's easy!
       
        let result = insert tree
        match result with
        |Success(node) -> node
        |Split(left,key,right) -> Structure([Double(key,left,right)])
       

        let Remove tree key = 
            //TODO     