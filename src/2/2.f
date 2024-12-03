      module security
      contains
        subroutine safety(report, i, unsafe, ncol)
          character*200, dimension(:), allocatable :: reports
          real*16, dimension(:), allocatable :: report
          real*16, dimension(:), allocatable :: reportnew
          integer lines, ncol, io, i, j, totSafe
          logical unsafe, decreasing

          unsafe = .FALSE.
          decreasing = .FALSE.
          ! For each level of the report
          do j = 1, (ncol-1)
          !write (*,*) i, j, ncol
            if (report(j) == report(j+1)) then
              unsafe = .TRUE.
              exit
            else if (report(j) > report(j+1)) then
              if (j == 1) then
                decreasing = .TRUE.
              else
                if (decreasing .eqv. .FALSE.) then
                  unsafe = .TRUE.
                end if
              end if
            else 
              if (j == 1) then
                decreasing = .FALSE.
              else
                if (decreasing) then
                  unsafe = .TRUE.
                end if
              end if
            end if

            if ((abs(report(j) - report(j+1))) > 3) then
              unsafe = .TRUE.
            else if ((abs(report(j) - report(j+1))) < 1) then
              unsafe = .TRUE.
            end if      
          end do
          !write(*,*) 'Subr ', unsafe, ':', report
        end subroutine
      end module





      program two
        use security
        character*200, dimension(:), allocatable :: reports
        real*16, dimension(:), allocatable :: report
        real*16, dimension(:), allocatable :: reportnew
        integer lines, ncol, io, i, j, k, totSafe
        logical unsafe, decreasing
        open(unit=12, file='input.txt')
        
        ! Count number of items to allocate
        io = 0
        lines = 0
        do while (io == 0)
          read(12, *, iostat=io)
          lines = lines + 1
        end do
        lines = lines - 1
        allocate(reports(lines))
        rewind(12) ! Start over from 1st line

        ! Read each entire line (report) into a variable, as a string
        read(12, '(A)') reports
        close(12)

        totSafe = 0
        ! For each report in reports
        do i = 1, lines
          ! Count the number of columns (levels) and allocate 'report' to that
          io = 0
          ncol = 1
          do while (io == 0)
            if (allocated(report)) deallocate(report)
            allocate(report(ncol))
            read(reports(i), *, iostat=io) report
            !write(*,*) report
            ncol = ncol + 1
          end do
          ncol = ncol - 2 ! Why -2 and not -1? Why does it add an extra 0 at the end?
          !write(*,*) 'ncol = ', ncol
          if (allocated(report)) deallocate(report)
          allocate(report(ncol))

          ! Read each level of the report to the report vector, as a real*16
          read(reports(i), *) report
          call safety(report, i, unsafe, ncol)
          
          if (unsafe) then
            write(*,*) 'Unsafe: ', reports(i)

            ! Part Two
            if (allocated(reportnew)) deallocate(reportnew)
            allocate(reportnew(ncol-1))
            do j = 1, ncol
              do k = 1, ncol
                if (k < j) then 
                  reportnew(k) = report(k)
                else if (k > j) then 
                  reportnew(k-1) = report(k)
                end if
              end do
              call safety(reportnew, i, unsafe, ncol-1)
              if (unsafe .eqv. .FALSE.) exit
            end do
            if (unsafe .eqv. .FALSE.) then
              write(*,*) 'Fixed - Safe: ', reportnew
              totSafe = totSafe + 1
            end if
          else
            write(*,*) 'Safe: ', reports(i)
            totSafe = totSafe + 1
          end if
        end do

        write(*,*) totSafe
      end program