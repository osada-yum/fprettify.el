# define are 3490
module       test_type_m
  implicit   none
  type                   ::test_type_t
     integer     ::      member
  end type test_type_t

contains

endmodule    test_type_m

program   test
             use     ,   intrinsic      ::   iso_fortran_env
use test_type_m
  implicit     none
     integer    ::               i     ,    j
 type(      test_type_t)    ::test_t
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

      print*,1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48

      test_t             %    member = 42

      write(   error_unit , ' ( i0   )'   ) test_t%member

  contains


     subroutine   nanika  (   arg )
       class(*)  ,    intent(  in  )            ::     arg
        select   type  (  arg   )
             type   is  (  integer  )
     print*,"hi"
           end select
     end     subroutine     nanika
end   program   test
