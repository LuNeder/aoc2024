      module sorting
      contains
      ! Sorts a list in crescent order
      subroutine sort(col, a, lines)
        real*16, dimension(:,:), allocatable :: a ! will allocate itself it seems
        real*16 aux
        integer lines, i, j, col
        do i = 1, lines
          do j = i+1, lines
            if (a(col, j) < a(col, i)) then
              aux = a(col, i)
              a(col, i) = a(col, j)
              a(col, j) = aux
            end if
          end do
        end do
      end subroutine
      end module

      ! implicit none defined by fpm.toml. Not sure if I like using implicit none tbh (implicit declaration is interesting), but at least I can't misspell a var without it warning me...
      program one
        use sorting
        real*16, dimension(:,:), allocatable :: lists
        real*16 totdist, simil
        integer lines, io, i, j, num

        open(unit=12, file='input.txt')
        
        ! Count number of items to allocate
        io = 0
        lines = 0
        do while (io == 0)
          read(12, *, iostat=io)
          lines = lines + 1
        end do
        lines = lines - 1
        allocate(lists(2, lines))
        
        rewind(12) ! Start over from 1st line
        read(12, *) lists ! Read right and left list from input
        close(12)

        !write(*,*) lists(1,:)
        !write (*,*) lists(2,:)
        
        ! Order
        call sort(1, lists, lines)
        call sort(2, lists, lines)
        
        ! Calculate total distance
        totDist = 0.0e0
        do i = 1, lines
          totDist = (abs(lists(1, i) - lists(2, i))) + totDist
        enddo

        write (*,*) totDist

        ! Part Two 
          ! real*16 because real (real*4?) gave me (rightAnswer-1), which is a wrong answer
        simil = 0.0e0
        do i = 1, lines
          num = 0.0e0
          do j = 1, lines
            if (lists(1, i) == lists(2,j)) num = num + 1
          end do
          simil = (lists(1, i) * num) + simil
        end do
        write (*,*) 'Similarity: ', simil
      end program
      
