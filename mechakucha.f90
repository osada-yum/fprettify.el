# define are 3490
program   test
             use     ,   intrinsic      ::   iso_fortran_env
  implicit     none
     integer    ::               i     ,    j

   print   *  ,   "hi"
i=   9
   do   i   =               1    ,9
      if    (i.le.    4  )      then
!print       *  ,  i, "a                                                                                                                    hi"
write ( *, * )                                  i
end if
 do j=1,2
write(*,*)j
    end do
      end do
                        ! print?
      print     *   ,   3    *    ( 1                   +2)    ,   .true.   .and.   .false..or..true.
      print*,+1,-9428

  contains


     subroutine   nanika  (   arg )
        integer  ,    intent(  in  )            ::     arg
        select   type  (  arg   )
             type   is  (  integer  )
     print*,"hi"
           end select
     end     subroutine     nanika
end   program   test
