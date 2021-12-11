!! This file is not dust but playground for `fprettify`.

#         define    are        3490
module       test_type_m
  use,intrinsic::       iso_c_binding        :    only         c_INT
  use, intrinsic :: ISO_FORTRAN_ENV
  implicit   none
  type                   ::test_type_t
     integer     ::      member
  end type test_type_t

  integer ( int32 )  ,   parameter  ,  private       ::    maxint   =    huge(0_int32)
  real ( 8 )  ,   parameter  ,  private       ::     pi  =    4*atan  (  1.0d0 )

contains

endmodule    test_type_m

program   test
             use     ,   intrinsic      ::   iso_fortran_env
use test_type_m
  implicit     none
real( real64 )   ,  allocatable ::            rs(:)
     integer    ::               i     ,    j
 type(      test_type_t)    ::test_t

       allocate(   rs ( 10   )  )

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
      print     *   ,   3    *    ( 1                   +2)    ,   .true.   .AND.   .false..or..TRUE.
      print*,+1,-9428

      !! if delete comment, this line fprettify warn.
      print*,1+2-3*4/5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+! 24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48

      test_t             %    member = 42

      test_t             %    member = 42 !&

      !&<
      test_t             %    member = 42
      write(   error_unit , ' ( i0   )'   ) test_t%member
      !&>
      write(   error_unit , ' ( i0   )'   ) test_t%member
      ! !&> ! <- error occur if only `!&>' exists.

      deallocate(   rs  )

  contains


     subroutine   nanika  (   arg )
       class(*)  ,    intent(  in  )            ::     arg
        select   type  (  arg   )
             type   is  (  integer  )
     print*,"hi"
           end select
     end     subroutine     nanika
end   program   test
