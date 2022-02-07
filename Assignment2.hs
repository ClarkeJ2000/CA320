
-- We need a test to see if number is Perfect first |


--function to find perfect numbers|
perfect :: Integral a => a -> Bool
perfect n = n == sumDivisors n

--Supposed to go up to 100,000,000,000
--having problem as doesn't seem to go that high|
upto :: Integral a => a -> Bool
upto n = filter perfect [1..n]


--struggled alot with next part 
-- alot of people online suggested a "SumDivisors" function 
-- Used online video for this 

sumDivisors a =
	--foldr starts from the right of my list and adds all values in it together|
	foldr(\n -> let (q,r) = a `quotRem` n in 
		if r == 0 then (+(n + q)) else id) 1
		[2 ..(floor . sqrt . fromIntegral $ a)]