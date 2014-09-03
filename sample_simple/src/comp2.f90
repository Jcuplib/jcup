program comp2
  use jcup_interface
  use common
  use jcup_interpolation_sample
  implicit none
  type(jcup_varg_type), pointer :: varg
  integer, pointer :: grid_index(:)
  integer, pointer :: send_grid(:), recv_grid(:)
  real(kind=8), pointer :: coef(:)
  real(kind=8), pointer :: data(:,:)
  real(kind=4), pointer :: data4(:,:)
  integer :: itime(8) = (/2014, 7, 7, 0, 0, 0, 0, 0/)
  integer :: delta_t = 400
  integer, parameter :: FID = 244
  integer :: i, j, k, t
  integer :: counter

  call jcup_set_new_comp(trim(COMP2_NAME))

  call jcup_initialize(COMP2_NAME, default_time_unit = "SEC", log_level = 2, log_stderr = .false.)
  !call jcup_initialize(COMP2_NAME, default_time_unit = "MIL", log_level = 0, log_stderr = .false.)

  allocate(grid_index(COMP2_NX*COMP2_NY))
  do j = 1, COMP2_NY
    do i = 1, COMP2_NX
      grid_index(i + COMP2_NX*(j-1)) = i + COMP2_NX*(j-1)
    end do
  end do

  call jcup_def_grid(grid_index, COMP2_NAME, COMP2_GRID, COMP2_NZ)
  call jcup_end_grid_def()
  
  call jcup_def_varg(varg, COMP2_NAME, "COMP2_VAR1", COMP2_GRID, COMP2_NZ, COMP1_NAME, "COMP1_VAR1", "SNP", interval = 1200, &
                     time_lag = -1, mapping_tag = 1, exchange_tag = 1)
  call jcup_end_var_def()

  call read_mapping_table("../src/mapping_table_1_to_2.txt", COMP1_NX, COMP2_NX, send_grid, recv_grid, coef)

  call jcup_set_mapping_table(COMP2_NAME, COMP1_NAME, COMP1_GRID, COMP2_NAME, COMP2_GRID, 1, send_grid, recv_grid)

  call jcup_init_time(itime)

  call set_operation_index(COMP2_NAME, COMP1_NAME, 1, coef)

  allocate(data(COMP2_NX*COMP2_NY,COMP2_NZ))
  allocate(data4(COMP2_NX*COMP2_NY,COMP2_NZ))

  open(unit = FID, file="comp2.dat", form="unformatted", status="replace", access="direct", recl=COMP2_NX*COMP2_NY*COMP2_NZ)

  do t = 1, 3
    data(:,:) = 0.d0
    call jcup_set_time(COMP2_NAME, itime, delta_t)


    call jcup_get_data(varg, data)

    data4 = data
    write(FID, rec=t, err=300) data4

    call jcup_inc_time(COMP2_NAME, itime)
  end do

  close(FID)

  call jcup_coupling_end(itime)
    
  stop

  300 continue
    write(0,*) "file write error"

end program

