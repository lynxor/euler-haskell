biggestPal :: (Integral a) => a -> String
biggestPal a = if palindrome (read a) then show a else biggestPal 
  where palindrome a = reverse a == a
        biggest x y = isPalindrome read (x * y)
        