      module sorting
      contains
      ! Sorts a list in crescent order
      subroutine sort(a, lines)
        real, dimension(:), allocatable :: a ! will allocate itself it seems
        real aux
        integer lines, i, j
        do i = 1, lines
          do j = i+1, lines
            if (a(j) < a(i)) then
              aux = a(i)
              a(i) = a(j)
              a(j) = aux
            end if
          end do
        end do
      end subroutine
      end module

      ! implicit none defined by fpm.toml. Not sure if I like using implicit none tbh (implicit declaration is interesting), but at least I can't misspell a var without it warning me...
      program one
        use sorting
        real, dimension(:), allocatable :: list1
        real, dimension(:), allocatable :: list2
        real, dimension(:,:), allocatable :: file2d
        real totdist
        integer lines, io, i

        open(unit=12, file='input.txt')
        
        ! Count number of items to allocate
        io = 0
        lines = 0
        do while (io == 0)
          read(12, *, iostat=io)
          lines = lines + 1
        end do
        lines = lines - 1
        allocate(list1(lines), list2(lines), file2d(2, lines))
        
        rewind(12) ! Start over from 1st line
        read(12, *) file2d ! Read right and left list from input
        close(12)
        
        ! I could probably clean this up to not require this, but I added the file2d temp var after the rest was done and I noticed the file wasn't being read correctly,
        ! so having a single 2d vector instead of two 1d vectors would mean I'd have to redo the sorting subroutine so nah...
        list1 = file2d(1, :)
        list2 = file2d(2, :)

        !write(*,*) list1
        !write (*,*) list2
        
        ! Order
        call sort(list1, lines)
        call sort(list2, lines)
        
        do i = 1, lines
          totDist = (abs(list1(i) - list2(i))) + totDist
        enddo


        !write(*,*) list1
        !write (*,*) list2
        write (*,*) totDist
      end program
      
