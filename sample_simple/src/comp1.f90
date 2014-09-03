program comp1
  use jcup_interface
  use common
  use jcup_interpolation_sample
  implicit none
  type(jcup_varp_type), pointer :: varp
  real(kind=8), pointer :: data(:,:)
  real(kind=4), pointer :: data4(:,:)
  integer :: itime(8) = (/2014, 7, 7, 0, 0, 0, 0, 0/)
  integer, pointer :: grid_index(:)
  integer :: i, j, k, t
  integer :: delta_t = 400
  integer(kind=8) :: sec_diff
  integer :: mil_diff, mcr_diff
  integer, parameter :: FID = 322

  call jcup_set_new_comp(trim(COMP1_NAME))
  call jcup_initialize(COMP1_NAME, default_time_unit = "SEC", log_level = 2, log_stderr = .false.)
  !call jcup_initialize(COMP1_NAME, default_time_unit = "MIL", log_level = 2, log_stderr = .true.)

  call set_grid_index(grid_index, COMP1_NX, COMP1_NY)

  call jcup_def_grid(grid_index, COMP1_NAME, COMP1_GRID, COMP1_NZ)
  call jcup_end_grid_def()

  call jcup_def_varp(varp, COMP1_NAME, "COMP1_VAR1", COMP1_GRID, COMP1_NZ)
  call jcup_end_var_def()
  
  call jcup_set_mapping_table(COMP1_NAME, COMP1_NAME, COMP1_GRID, COMP2_NAME, COMP2_GRID, 1)


  allocate(data(COMP1_NX*COMP1_NY, COMP1_NZ))
  allocate(data4(COMP1_NX*COMP1_NY, COMP1_NZ))
  
  call set_test_data(data, COMP1_NX, COMP1_NY, COMP1_NZ)

  !itime(:) = (/2014,7,7,0,0,1,200,0/)
  !call jcup_read_restart(255, itime)
  call jcup_init_time(itime)

  call jcup_put_data(varp, data)

  open(unit = FID, file="comp1.dat", form="unformatted", status="replace", access="direct", recl=COMP1_NX*COMP1_NY*COMP1_NZ)

  do t = 1, 3
    data4 = data
    write(FID, rec=t) data4

    call jcup_set_time(COMP1_NAME, itime, delta_t)

    call set_test_data(data, COMP1_NX, COMP1_NY, COMP1_NZ)

    call jcup_put_data(varp, data)
    call jcup_inc_time(COMP1_NAME, itime)
  end do

  !call jcup_write_restart(255, itime)
  
  close(FID)

  call jcup_coupling_end(itime)
    
end program

