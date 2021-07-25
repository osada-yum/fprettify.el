program   test
  implicit none
     integer    ::               i
   print   *  ,   "hi"
i=   9
   do   i   =               1    ,9
      if    (i.le.    4  )      then
print       *  ,  i, "a                                                                                                                    hi"
write ( *, * )                                  i
   end if
      end do
end   program   test
