! Nothing more than a test bench.
   
   
   
   program main
   use geometry_mod
   use polygon_mod
   implicit none
   
   integer           :: unit
   real, allocatable :: x(:), y(:)
   integer           :: n, i
   type(Vertex)      :: v1, v2
   type(Edge), allocatable :: eds(:)
   type(Polygon)     :: poly
   type(Edge)        :: ed
   
   open(newunit = unit, file='C:\Users\al064648\Desktop\asymmetric_conduit_profile.dat', status='old')
   
   read(unit, '(I5)') n
   
   allocate(x(n), y(n))
   
   do i = 1, n
      read(unit, *) x(i), y(i)
   end do
   
   do i = 1, n-1
      v1 = new_Vertex(x(i), y(i))
      v2 = new_Vertex(x(i+1), y(i+1))
      ed = new_Edge(v1, v2)
      call add_to_Polygon(poly, ed)
   end do
   v1 = new_Vertex(x(n), y(n))
   v2 = new_Vertex(x(1), y(1))
   ed = new_Edge(v1, v2)
   call add_to_Polygon(poly, ed)
   
   call dump_Polygon(poly)
   
   pause
   
   end program main
   
   
   