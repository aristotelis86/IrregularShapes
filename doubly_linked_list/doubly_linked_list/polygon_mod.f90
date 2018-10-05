! Module to introduce the shape of the conduit 
! unit and relevant methods operating on it.
   
   
   
   module polygon_mod
   use geometry_mod
   use doubly_linked_list_mod
   
   implicit none
   private
   
   public      :: Polygon, add_to_Polygon, dump_polygon
   
   type Polygon
      type(DLinkList)   :: edges
   end type
   
   contains
   ! 
   !===========================================================
   ! 
   elemental function new_Polygon(ed) result(poly)
   type(Polygon)                 :: poly
   type(Edge), intent(in)        :: ed
   
   call append(poly%edges, ed)
   end function new_Polygon
   
   ! 
   !===========================================================
   ! 
   subroutine add_to_Polygon(poly, ed)
   type(Polygon), intent(inout)     :: poly
   type(Edge), intent(in)           :: ed
   
   call append(poly%edges, ed)
   end subroutine add_to_Polygon
   
   ! 
   !===========================================================
   ! 
   subroutine dump_Polygon(poly)
   type(Polygon), intent(in)        :: poly
   
   type(Edge), pointer  :: nod
   integer              :: i, n
   
   n = get_num_nodes(poly%edges)
   write(*,'(''The polygon has '', I5, '' edges.'')') n
   write(*,'(''---------------------------------'')')
   do i = 1, n
      nod = get_node(poly%edges, i)
      call dump_Edge(nod)
   end do
   
   end subroutine dump_Polygon
   
   end module polygon_mod
   
   