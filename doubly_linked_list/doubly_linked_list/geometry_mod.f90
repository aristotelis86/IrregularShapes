! Module introducing the geometric
! elements needed to construct the 
! irregular conduit shapes.
   
   module geometry_mod
   implicit none
   
   private
   
   public   :: Vertex, Edge, new_Vertex, new_Edge, dump_Vertex, dump_Edge
   
   type Vertex
      private
      real           :: x
      real           :: y
   end type
   
   type Edge
      private
      type(Vertex)          :: v1
      type(Vertex)          :: v2
   end type
   
   contains
   ! 
   !===========================================================
   ! 
   elemental function new_Vertex(x, y) result(vert)
   type(Vertex)      :: vert
   real, intent(in)  :: x, y
   vert%x = x
   vert%y = y
   end function new_Vertex
   
   ! 
   !===========================================================
   ! 
   elemental subroutine modify_Vertex(vert, x, y) 
   type(Vertex), intent(inout)      :: vert
   real, intent(in)                 :: x, y
   vert%x = x
   vert%y = y
   end subroutine modify_Vertex
   
   ! 
   !===========================================================
   ! 
   subroutine dump_Vertex(vert)
   type(Vertex), intent(in)      :: vert
   write(*,'(''('',F10.3,'','',F10.3,'')'')') vert%x, vert%y
   end subroutine dump_Vertex
   
   ! 
   !===========================================================
   ! 
   elemental function new_Edge(v1, v2) result(ed)
   type(Edge)        :: ed
   type(Vertex), intent(in)   :: v1, v2
   ed%v1 = v1
   ed%v2 = v2
   end function new_Edge
   
   ! 
   !===========================================================
   ! 
   elemental subroutine modify_Edge(ed, v1, v2)
   type(Edge), intent(inout)     :: ed
   type(Vertex), intent(in)      :: v1, v2
   ed%v1 = v1
   ed%v2 = v2
   end subroutine modify_Edge
   
   ! 
   !===========================================================
   ! 
   subroutine dump_Edge(ed)
   type(Edge), intent(in)      :: ed
   write(*,'(''('',F10.3,'','',F10.3,'') ----- ('',F10.3,'','',F10.3,'')'')') ed%v1%x, ed%v1%y, ed%v2%x, ed%v2%y
   end subroutine dump_Edge
   
   
   end module geometry_mod
   