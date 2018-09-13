! Before calling this function, we need to make sure
! that the arrays provide a counter-clockwise representation
! of the curve AND the curve is not self intersecting
function calculate_area_random_shape(x, y) result(val)

implicit none

real, intent(in)           :: x(:), y(:)
real                       :: val

integer                    :: n, if

! Get the size of input arrays
n = size(x)

if (n /= size(y)) then
   ! error
end if

val = 0.0
do i = 1, n-1
   x1 = x(i)
   x2 = x(i+1)
   y1 = y(i)
   y2 = y(i+1)

   val = val + x2*(y2-y1) - y2*(x2-x1)
end do

x1 = x(n)
x2 = x(1)
y1 = y(n)
y2 = y(1)

val = val + x2*(y2-y1) - y2*(x2-x1)

val = val/2.
end function calculate_area_random_shape






