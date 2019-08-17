exception ThereIsNoSolution;;

let hittingString stringList = (*a set of strings made of '0', '1' and '2'*)
	let stringLength = String.length (List.hd stringList) in (*length of each string in the stringList*)
	let filteredStringList = List.filter (fun x -> not(String.contains x '2')) stringList in (*eliminating from the stringList strings containing '2'*)
	let lengthFilteredList = List.length filteredStringList in (*number of strings in the filteredStringList*)
	let partialWeightedSolution subLength = (*calculating how many substrings of length <sublength> of the filteredStringList are equal to a string <x>*)
											begin fun x -> 
												let subsubList =
													List.map
														(fun y -> String.compare x y = 0)
														(List.map (fun x -> String.sub x 0 subLength) filteredStringList)

												in
												(x, List.length (List.filter (fun z -> z) subsubList)) 
											end
    
									in
	let extendSolution =
						fun x -> [x^"0";x^"1"] in
	let rec aux step solutionsList = if step > stringLength or filteredStringList = [] then raise ThereIsNoSolution
									 else
											let heaD = List.hd solutionsList in
											let taiL = List.tl solutionsList in
												if snd (partialWeightedSolution step heaD) = 0 then 
													if step < stringLength then 
														let difference = stringLength - step in
															heaD^(String.make difference 'x')
													else heaD
												else 
													let extension = extendSolution heaD in 
													aux (String.length (List.hd taiL)) (taiL@extension)
									 in aux 1 ["0";"1"]
									 
									 