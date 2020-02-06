!====================================================================================================
!> @brief
!> jcup interface module 
!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_interface 
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA, NUM_OF_EXCHANGE_GRID, REAL_DATA, DOUBLE_DATA
  use jcup_constant, only : DATA_1D, DATA_2D, DATA_25D, DATA_3D
  use jcup_constant, only : NAME_LEN
  use jcup_mpi_lib, only : jcup_set_world => jml_set_global_comm
  use jcup_mpi_lib, only : jcup_get_world => jml_get_global_comm
  use jcup_mpi_lib, only : jcup_get_myrank_global => jml_GetMyrankGlobal
  use jcup_mpi_lib, only : jcup_get_leader_rank => jml_GetLeaderRank
  use jcup_mpi_lib, only : jcup_get_comm_size => jml_GetCommSizeLocal
  use jcup_comp, only : jcup_get_num_of_component => get_num_of_total_component
  use jcup_comp, only : jcup_get_component_name => get_component_name
  use jcup_comp, only : jcup_is_my_component => is_my_component
  use jcup_comp, only : jcup_get_comp_num_from_name => get_comp_id_from_name
  use jcup_comp, only : jcup_is_model_running => is_model_running
  use jcup_time, only : time_type
  use jcup_config, only : jcup_get_num_of_send_data => get_num_of_send_data 
  use jcup_config, only : jcup_get_send_data_name => get_my_send_data_name
  use jcup_config, only : jcup_get_num_of_recv_data => get_num_of_recv_data 
  use jcup_config, only : jcup_get_recv_data_name => get_my_recv_data_name
  use jcup_data, only : jcup_varp_type => varp_type
  use jcup_data, only : jcup_varg_type => varg_type
  use jcup_grid_base, only : jcup_get_grid_info => get_grid_info
  use jcup_intercomm, only : jcup_init_advanced_exchange => init_advanced_exchange
  use jcup_exchange, only : jcup_send_data_immediately, jcup_recv_data_immediately, &
                            jcup_put_data_1d_double, jcup_put_data_25d_double, &
                            jcup_get_data_1d_double, jcup_get_data_25d_double
  use jcup_interpolation, only : interpolate_data
  use jcup_interpolation_interface, only : jcup_get_local_operation_index  => get_local_operation_index
  use jcup_interpolation_interface, only : jcup_get_send_grid_index => get_send_grid_index 
  use jcup_interpolation_interface, only : jcup_get_recv_grid_index => get_recv_grid_index
  use jcup_interpolation_interface, only : jcup_get_num_of_send_grid => get_num_of_send_grid
  use jcup_interpolation_interface, only : jcup_get_num_of_recv_grid => get_num_of_recv_grid
  use jcup_interpolation_interface, only : jcup_send_array => send_array_to_recv_model
  use jcup_interpolation_interface, only : jcup_recv_array => recv_array_from_send_model
  use jcup_interpolation_interface, only : jcup_send_coef => send_coef_to_recv_model
  use jcup_interpolation_interface, only : jcup_recv_coef => recv_coef_from_send_model
  use jcup_interpolation_interface, only : jcup_set_local_coef => set_local_coef
  use jcup_interpolation_interface, only : jcup_set_coef_local_to_local => set_coef_local_to_local
  use jcup_interpolation_interface, only : OPERATION_COEF, SEND_COEF, RECV_COEF
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: jcup_set_world    ! subroutine (global_comm)
  public :: jcup_get_world    ! integer function()
  public :: jcup_set_new_comp ! subroutine (component_name)
  public :: jcup_initialize   ! subroutine (component_name, default_time_unit, log_level, log_stderr) ! 2014/07/11 [MOD]
  public :: jcup_coupling_end ! subroutine (time_array, is_call_mpi_finalize)

  public :: jcup_log          ! subroutine (sub_name, log_string, log_level)
  public :: jcup_error        ! subroutine (sub_name, error_string)
  public :: jcup_suspend_log  ! subroutine ()
  public :: jcup_get_mpi_parameter ! subroutine (com_name, my_comm, my_group, my_size, my_rank)
  public :: jcup_get_model_id      ! subroutine (model_name, model_id)

  public :: jcup_get_myrank_global ! integer function (NONE)
  public :: jcup_get_leader_rank   ! integer function (component_id)
  public :: jcup_get_comm_size     ! integer function (component_id)
  public :: jcup_get_num_of_component ! integer function (NONE)
  public :: jcup_is_model_running     ! logical function (COMP_NAME)

  public :: jcup_get_component_name   ! character(len=name_len) function (component_id)
  public :: jcup_is_my_component ! logical function (component_id)
  public :: jcup_def_grid          ! subroutine (grid_index, model_name, grid_name, num_of_vgrid)
  public :: jcup_end_grid_def      ! subroutine ()
  public :: jcup_get_grid_info     ! subroutine (comp_name, grid_name, num_of_index, min_index, max_index) 2013.09.20 [ADD]
  public :: jcup_set_default_configuration ! subroutine (my_comp_name, send_comp_name, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  public :: jcup_def_varp          ! subroutine (data_type_ptr, comp_name, data_name, grid_name, num_of_data)
  public :: jcup_def_varg ! subroutine (data_type_ptr, comp_name, data_name, grid_name, num_of_data, 
                          ! send_model_name, send_data_name, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  public :: jcup_end_var_def       ! subroutine ()

  public :: jcup_init_time         ! subroutine (time_array) :: integer time_array(6)
  public :: jcup_set_time          ! subroutine (component_name, time_array, delta_t, is_exchange)
                                   ! subroutine (component_name, time_real, delta_t)  ! 2013.0910 [ADD]
  public :: jcup_set_mapping_table ! subroutine (my_comp_name, send_comp_name, send_grid_name, recv_comp_name, recv_grid_name, 
                                   !             mapping_tag, send_grid_index, recv_grid_index)
  public :: jcup_set_mapping_table_local ! subroutine (my_comp_name, send_comp_name, send_grid_name, recv_comp_name, recv_grid_name, 
                                   !                   mapping_tag, num_of_grid, send_grid_index, send_pe_num, recv_grid_index) 
  public :: jcup_inc_time          ! subroutine (component_name, time_array)
  public :: jcup_inc_calendar      ! subroutine (time_array, delta_t) 2014/11/13 [ADD]
#ifndef NO_F2003
  public :: jcup_put_value         ! subroutine (data_type, data) 2015/02/23 [ADD]
  public :: jcup_get_value         ! subroutine (data_type, data) 2015/02/23 [ADD]
#endif
  public :: jcup_put_data          ! subroutien (data_type, data, num_of_data)
  public :: jcup_get_data          ! subroutine (data_type, data, num_of_data, is_recv_ok)

  public :: jcup_write_restart     ! subroutine (file_id, time_array) 2013.05.29 [ADD]
  public :: jcup_read_restart      ! subroutine (file_id, time_array) 2013.05.29 [ADD]

  public :: jcup_init_advanced_exchange ! subroutine (num_of_family_members) 2017/02/14 [ADD]
  public :: jcup_send_data_immediately
  public :: jcup_recv_data_immediately

  public :: jcup_get_comp_num_from_name ! integer function (component_name)
  public :: jcup_get_num_of_send_data ! integer function (compnent_id or component_name or current_component)
  public :: jcup_get_send_data_name   ! character(len=NAME_LEN) function (component_name, data_index)
  public :: jcup_get_num_of_recv_data ! integer function (compnent_id or component_name or current_component)
  public :: jcup_get_recv_data_name   ! character(len=NAME_LEN) function (component_name, data_index)
  
  public :: jcup_varp_type
  public :: jcup_varg_type
  public :: jcup_get_local_operation_index ! subroutine (recv_comp_name, send_comp_name, tag, num_of_operation, operation_index,  
                                           !             send_data_index, recv_data_index, send_coef_index, recv_coef_index)
  public :: jcup_get_send_grid_index ! subroutine (recv_comp_id, send_cpmp_id, grid_tag, num_of_grid, grid_index) 2013.06.21 [ADD]
  public :: jcup_get_recv_grid_index ! subroutine (recv_comp_id, send_cpmp_id, grid_tag, num_of_grid, grid_index) 2013.06.21 [ADD]
  public :: jcup_get_num_of_send_grid
  public :: jcup_get_num_of_recv_grid
  public :: jcup_send_array ! subroutine (my_comp_name, recv_comp_name, array)
  public :: jcup_recv_array ! subroutine (my_comp_name, send_comp_name, array)
  public :: jcup_send_coef  ! subroutine (my_comp_name, recv_comp_name, coef)
  public :: jcup_recv_coef  ! subroutine (my_comp_name, send_comp_name, coef)
  public :: jcup_set_local_coef ! subroutine (my_comp_name, send_comp_name, mapping_tag, global_coef, local_coef, operation_type)
  public :: jcup_set_coef_local_to_local ! subroutine (my_comp_name, my_grid_name, send_comp_name, mapping_tag, 
                                         !             local_array_coef, local_intpl_coef, coef_type)
  public :: OPERATION_COEF, SEND_COEF, RECV_COEF

!--------------------------------   private  ---------------------------------!

  integer, private :: my_coupler
  integer, private :: current_domain

  logical, private :: is_Initialized
  logical, private :: is_SetGrid
  logical, private :: is_InitTime
  logical, private :: is_SetMappingTable
  logical, private :: is_Initialize_completed
  logical, private :: is_EndDef
  logical, private :: is_EndVarDef
  logical, private :: is_init_conf 
  logical, private :: is_restart  ! 2014/07/15 [ADD]
  logical, private :: is_final_send ! 2014/12/08 [ADD]


  interface jcup_init_time
    module procedure jcup_init_time_int
  end interface

  interface jcup_set_time
    module procedure jcup_set_date_time_int !, jcup_set_date_time_real
  end interface

  interface jcup_put_data
    module procedure  jcup_put_data_1d, jcup_put_data_25d
  end interface

  interface jcup_get_data
    module procedure jcup_get_data_1d, jcup_get_data_25d
  end interface



  integer, private :: max_i_1d


  integer, private :: rec_counter 

  type(time_type), save, private :: current_time

  integer :: max_num_of_exchange_data

  character(len=NAME_LEN) :: my_model_name

  logical, save, private :: is_sync_exchange = .false.
  logical, save, private :: is_assync_exchange = .false.
  
contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set component name
!> @param[in] component_name name of component
subroutine jcup_set_new_comp(component_name)
  use jcup_comp, only : set_my_component
  implicit none
  character(len=*), intent(IN) :: component_name

  call set_my_component(component_name)

end subroutine jcup_set_new_comp

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize Jcup
!> @param[in] model_name model name
!> @param[in] inCallInit flag call MPI_Init or not
! 2014/07/11 [MOD] add default_time_unit
! 2014/08/27 [MOD] delete argument isCallInit
! 2014/12/10 [MOD] add component allocation check
subroutine jcup_initialize(model_name, default_time_unit, log_level, log_stderr)
  use jcup_config, only : init_conf
  use jcup_comp, only : init_model_process, get_num_of_total_component, is_my_component, get_component_name
  use jcup_utils, only : set_log_level, init_log, put_log, IntToStr
  use jcup_buffer, only : init_buffer, buffer_check_write
  use jcup_time, only : time_type, init_all_time, init_each_time, set_time_data, TU_SEC, TU_MIL, TU_MCR, &
                        set_time_unit, get_time_unit
  use jcup_data, only : init_data_def
  use jcup_grid, only : init_grid
  use jcup_mpi_lib, only : jml_abort, jml_AllreduceMin, jml_AllreduceMax, jml_AllreduceMaxLocal
  use jcup_exchange, only : init_exchange
  use jal_api, only : jal_init
  implicit none
  character(len=*),intent(IN) :: model_name ! main component name of my task 
  character(len=3), optional, intent(IN) :: default_time_unit ! 2014/07/03 [ADD]
  integer, optional, intent(IN) :: log_level ! 0, 1, 2
  logical, optional, intent(IN) :: log_stderr 
  integer :: num_of_comp 
  type(time_type) :: time
  integer :: mdl
  integer :: opt_log_level
  logical :: opt_log_stderr
  integer :: my_time_unit, tu_min, tu_max
  integer :: my_comp, max_comp
  integer :: i

  is_InitTime = .false.
  is_SetGrid = .false.
  is_SetMappingTable = .false.
  is_Initialize_completed = .false.
  is_EndDef = .false.
  is_EndVarDef = .false.
  is_init_conf = .false.
  is_restart   = .false. ! 2014/07/15 [ADD]
  is_final_send = .true. ! 2014/12/08 [ADD]
 
  call init_model_process() ! 2014/08/27 [MOD]

  ! 2014/12/10 [ADD] component allocation check
  !my_comp = 0
  !do i = 1, get_num_of_total_component()
  !  if (is_my_component(i)) my_comp = my_comp + 1
  !end do
  !do i = 1, get_num_of_total_component()
  !  if (is_my_component(i)) then
  !    call jml_AllReduceMaxLocal(i, my_comp, max_comp)
  !    if (my_comp /= max_comp) then
  !      write(0, *) "jcup_initialize, component allocation error, component id = ", i, "num of comp, max comp = ", my_comp, max_comp
  !      call mpi_finalize(i)
  !      stop
  !    end if
  !  end if
  !end do

 
  ! set time unit
  if (present(default_time_unit)) then
    select case(default_time_unit)
    case("SEC")
      call set_time_unit(TU_SEC)
    case("MIL")
      call set_time_unit(TU_MIL)
    case("MCR")
      call set_time_unit(TU_MCR)
    case default
      write(0, *) "jcup_initialize, default_time_unit setting error!!!, "//trim(default_time_unit)
      stop 999
    end select
  else
    call set_time_unit(TU_SEC)
  end if

  ! check time unit of all components
  my_time_unit = get_time_unit()
  
  call jml_AllreduceMin(my_time_unit, tu_min)
  call jml_AllreduceMax(my_time_unit, tu_max)
  
  if (tu_min /= tu_max) then
    write(0,*) "jcup_intialize, the time unit must be same in whole component"
    call jml_abort()
    stop 9999 
  end if


  num_of_comp = get_num_of_total_component()

  max_num_of_exchange_data = NUM_OF_EXCHANGE_DATA ! set initial value 2013/04/02 

  call init_all_time(num_of_comp)

  do mdl = 1, num_of_comp
    call init_each_time(mdl, 1) ! the number of domain is set to 1
  end do

  call init_conf(num_of_comp)

  if (present(log_level)) then
    opt_log_level = log_level
  else
    opt_log_level = 0 ! default no output log
  end if

  if (present(log_stderr)) then
    opt_log_stderr = log_stderr
  else
    opt_log_stderr = .false. ! default no output stderr
  end if

  call set_log_level(opt_log_level, opt_log_stderr)

  call init_log(trim(model_name))


  call init_buffer()

  call init_exchange(num_of_comp)

  call init_grid()

  is_Initialized = .true.

  rec_counter = 0

  max_i_1d = 1

  call set_time_data(current_time, 0, 0, 0, 0, 0, int(0, kind=8))

  current_time%delta_t = 0

  call put_log("coupler initialization compoleted ", 1)

  do mdl = 1, get_num_of_total_component()
    if (is_my_component(mdl)) then
      call put_log("assigned component name : "//trim(get_component_name(mdl))//", comp_id = "//trim(IntToStr(mdl)))
    end if
  end do

  my_model_name = model_name

  call jal_init(my_model_name)
  
end subroutine jcup_initialize

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> finalize Jcup
!> @param[in] time_array end time
!> @param[in] inCallFinalize flag call MPI_Finalize or not
! 2014/07/08 [MOD] time_array(6) -> time_array(:)
! 2014/08/27 [MOD] delete argument isCallFinalize
! 2014/11/14 [MOD] call jcup_set_time -> current_comp_id = i
! 2014/12/08 [MOD] add if (.not.is_my_component) 
! 2015/04/02 [ADD] add recv_all_scalar_data
! 2015/11/24 [MOD] time_array -> optional
subroutine jcup_coupling_end(time_array, isCallFinalize)
  use jcup_mpi_lib, only : jml_finalize!, jml_Send1D_m2c, jml_destruct_window
  use jcup_time, only : destruct_all_time
  use jcup_utils, only : finalize_log, put_log
  use jcup_buffer, only : buffer_check_write, destruct_buffer
  use jcup_exchange, only : recv_all_scalar_data, finalize_exchange, send_final_step_data
  use jal_api, only : jal_finish
  implicit none
  integer, optional, intent(IN) :: time_array(:) ! 2014/07/08
  logical, optional, intent(IN) :: isCallFinalize
  logical :: is_call_finalize
  integer :: i

  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!   COUPLER FINALIZE  !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)

  call jal_finish()
  
  call send_final_step_data()
  
  call destruct_buffer()

  call destruct_all_time()

  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!  COUPLING COMPLETED !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


  if (is_final_send) then
    call finalize_log()
  end if

  call finalize_exchange()


  if (present(isCallFinalize)) then
    is_call_finalize = isCallFinalize
  else
    is_call_finalize = .true.
  end if

  call recv_all_scalar_data() ! 2015/04/02 [ADD]
  call jml_finalize(is_call_finalize)

end subroutine jcup_coupling_end

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> abort jcup
!> @param[in] sub_name subroutine name
!> @param[in] error_str error message string
!> @param[in] log_level log level (1:standard, 2:detail)
subroutine jcup_log(sub_name, error_str, log_level)
  use jcup_utils, only : put_log
  character(len=*), intent(IN) :: sub_name
  character(len=*), intent(IN) :: error_str
  integer, optional, intent(IN) :: log_level
  integer :: ll

  ll = 2
  if (present(log_level)) ll = log_level

  call put_log("Sub["//trim(sub_name)//"] : "//trim(error_str), ll)

end subroutine jcup_log

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> abort jcup
!> @param[in] sub_name subroutine name
!> @param[in] error_str error message string

subroutine jcup_error(sub_name, error_str)
  use jcup_utils, only : Error
  character(len=*), intent(IN) :: sub_name
  character(len=*), intent(IN) :: error_str

  call Error(sub_name, error_str)

end subroutine jcup_error

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> suspend log output

subroutine jcup_suspend_log()
  use jcup_utils, only : set_log_level, NO_OUTPUT_LOG, NO_OUTPUT_STDERR

  call set_log_level(NO_OUTPUT_LOG, NO_OUTPUT_STDERR)

end subroutine jcup_suspend_log

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get mpi parameters
!> @param[in] comp_name name of component
!> @param[out] my_comm communicator of the component
!> @param[out] my_group mpi group of the component
!> @param[out] my_size mpi size of the component
!> @param[out] my_rank mpi rank of the component
subroutine jcup_get_mpi_parameter(comp_name, my_comm, my_group, my_size, my_rank)
  use jcup_mpi_lib, only : jml_GetComm, jml_GetMyGroup, jml_GetCommSizeLocal, jml_GetMyRank, jml_GetCommNULL
  use jcup_comp, only : get_comp_id_from_name, is_my_component
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer, intent(OUT) :: my_comm, my_group, my_size, my_rank
  integer :: comp_id

  if (trim(comp_name)=="GLOBAL") then
    call jcup_get_mpi_parameter_global(my_comm, my_size, my_rank)
    my_group = 0
  else

    comp_id = get_comp_id_from_name(comp_name)

    if (is_my_component(comp_id)) then
      my_comm = jml_GetComm(comp_id) 
    else
      my_comm = jml_GetCommNULL()
    end if

    my_group = jml_GetMyGroup(comp_id) 
    my_size  = jml_GetCommSizeLocal(comp_id) 
    my_rank  = jml_GetMyRank(comp_id)

  end if

end subroutine jcup_get_mpi_parameter

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_get_mpi_parameter_global(my_comm, my_size, my_rank)
  use jcup_mpi_lib, only : jml_GetCommGlobal, jml_GetCommSizeGlobal, jml_GetMyrankGlobal
  integer, intent(OUT) :: my_comm, my_size, my_rank

  my_comm = jml_GetCommGlobal()
  my_size = jml_GetCommSizeGlobal()
  my_rank = jml_GetMyrankGlobal()

end subroutine jcup_get_mpi_parameter_global

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> return my component id number 
!> @param[in] comp_name name of component
!> @param[out] model_id component id number 
subroutine jcup_get_model_id(model_name, model_id)
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: model_name
  integer, intent(OUT) :: model_id

  model_id = get_comp_id_from_name(model_name)

end subroutine jcup_get_model_id

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set grid definition
!> @param[in] grid_index array of grid indexes
!> @param[in] model_name name of component
!> @param[in] grid_name name of grid
!> @param[in] num_of_vgrid number of data or number of vertical grid
subroutine jcup_def_grid(grid_index, model_name, grid_name, num_of_vgrid)
  use jcup_comp, only : get_num_of_my_component, is_my_component
  use jcup_grid, only : def_grid
  use jcup_utils, only : error, put_log, IntToStr
  implicit none
  integer, intent(IN) :: grid_index(:)
  character(len=*), intent(IN) :: model_name ! model (component) name
  character(len=*), intent(IN) :: grid_name
  integer, optional, intent(IN) :: num_of_vgrid
  integer :: i

  if (.not.is_my_component(model_name)) then
    call error("jcup_def_grid", "Component name : "//trim(model_name)//" is not defined")
  end if
  
  if (minval(grid_index) <= 0) then
    call error("jcup_def_grid", "grid index must be >= 1")
  end if

  if (present(num_of_vgrid)) then
    if (num_of_vgrid > max_num_of_exchange_data) then
      max_num_of_exchange_data = num_of_vgrid
    end if
  end if

  call def_grid(grid_index, model_name, grid_name)

  max_i_1d = max(max_i_1d, size(grid_index))

  call put_log("jcup_def_grid : component name : "//trim(model_name)//", grid name : "//trim(grid_name)//", grid size : " &
                                //trim(IntToStr(size(grid_index))) &
               //", min : "//trim(IntToStr(minval(grid_index)))//", max : "//trim(IntToStr(maxval(grid_index))))

end subroutine jcup_def_grid

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> finalize grid definition
subroutine jcup_end_grid_def()
  use jcup_mpi_lib, only : jml_AllreduceMax
  use jcup_utils, only : put_log, IntToStr
  use jcup_grid, only : end_def
  use jcup_exchange, only : init_buffer_1d
  implicit none
  integer :: grd
  integer :: int_buffer(1)

  if (.not.is_Initialized) then
    call jcup_abnormal_end("jcup_SetGrid","jcup_Initialize not called")
  end if

  int_buffer(1) = max_num_of_exchange_data

  call jml_AllreduceMax(int_buffer(1), NUM_OF_EXCHANGE_DATA)

  call put_log("jcup_end_grid_def : NUM_OF_EXCHANGE_DATA = "//trim(IntToStr(NUM_OF_EXCHANGE_DATA)))

  call end_def()

  call init_buffer_1d(max_i_1d, NUM_OF_EXCHANGE_DATA)

  is_EndDef = .true.
  is_SetGrid = .true.

  call put_log("Grid definition completed")

end subroutine jcup_end_grid_def

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define send data
!> @param[inout] data_type_ptr pointer of data_type variable
!> @param[in] data_name name of send/recv data
!> @param[in] grid_name name of grid 
!> @param[in] num_of_data number of data
subroutine jcup_def_varp(data_type_ptr, comp_name, data_name, grid_name, num_of_data)
  use jcup_constant, only : NO_GRID, STRING_LEN
  use jcup_utils, only : error, IntToStr
  use jcup_data, only : varp_type, def_varp
  use jcup_grid, only : get_my_grid_num, is_my_grid
  use jcup_data, only : init_data_def 
  use jcup_exchange, only : init_buffer_25d
  implicit none
  type(varp_type), pointer :: data_type_ptr
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: data_name
  character(len=*), intent(IN) :: grid_name
  integer, optional, intent(IN) :: num_of_data
  character(len=STRING_LEN) :: logstr
  integer :: grid_id

  if (index(data_name, "__") > 0) then
    call error("jcup_def_varp", "string __ is not allowed for data_name")
  end if

  if (.not.is_init_conf) then
    call init_data_def()
    is_init_conf = .true.
  end if

  if (.not.is_my_grid(grid_name)) then
    call error("jcup_def_varp", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  grid_id = get_my_grid_num(grid_name)

  if (grid_id==NO_GRID) then
    call error("jcup_def_varp", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  if (present(num_of_data)) then

    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
      logstr = "parameter num_of_data must be <= NUM_OF_EXCHANGE_DATA. num_of_data = " &
               //trim(IntToStr(num_of_data))
      call error("jcup_def_varp", trim(logstr))
    end if

    if (num_of_data > 1) then
      call init_buffer_25d(num_of_data)
    end if
    call def_varp(data_type_ptr, comp_name, data_name, grid_id, num_of_data)
  else 
    call def_varp(data_type_ptr, comp_name, data_name, grid_id)
  end if

end subroutine jcup_def_varp

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define recv data
!> @param[in] my_comp my component name
!> @param[in] send_com send component name
!> @param[in] recv_mode recieve mode
!> @param[in] interval recieve interval
!> @param[in] time_lag time lag
!> @param[in] mapping_tag mapping tag
!> @param[in] exchange_tag exchange_tag
subroutine jcup_set_default_configuration(my_comp, send_comp, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  use jcup_data, only : init_data_def, set_default_config
  implicit none
  character(len=*), intent(IN) :: my_comp, send_comp
  character(len=3), optional, intent(IN) :: recv_mode
  integer, optional, intent(IN) :: interval
  integer, optional, intent(IN) :: time_lag
  integer, optional, intent(IN) :: mapping_tag
  integer, optional, intent(IN) :: exchange_tag

  if (.not.is_init_conf) then
    call init_data_def()
    is_init_conf = .true.
  end if

  call set_default_config(my_comp, send_comp, recv_mode, interval, time_lag, mapping_tag, exchange_tag)

end subroutine jcup_set_default_configuration

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define recv data
!> @param[inout] data_type_ptr pointer of data_type variable
!> @param[in] data_name name of send/recv data
!> @param[in] grid_name name of grid 
!> @param[in] num_of_data number of data
subroutine jcup_def_varg(data_type_ptr, comp_name, data_name, grid_name, num_of_data, &
     send_model_name, send_data_name, recv_mode, interval, time_lag, &
     mapping_tag, exchange_tag, time_intpl_tag)
  use jcup_constant, only : NO_GRID, STRING_LEN
  use jcup_utils, only : error, IntToStr
  use jcup_data, only : varg_type, def_varg
  use jcup_grid, only : get_my_grid_num, is_my_grid
  use jcup_data, only : init_data_def 
  use jcup_exchange, only : init_buffer_25d
  implicit none
  type(varg_type), pointer :: data_type_ptr
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: data_name
  character(len=*), intent(IN) :: grid_name
  integer, optional, intent(IN) :: num_of_data
  character(len=*), intent(IN) :: send_model_name
  character(len=*), intent(IN) :: send_data_name
  character(len=3), optional, intent(IN) :: recv_mode
  integer, optional, intent(IN) :: interval
  integer, optional, intent(IN) :: time_lag
  integer, optional, intent(IN) :: mapping_tag
  integer, optional, intent(IN) :: exchange_tag
  integer, optional, intent(IN) :: time_intpl_tag ! time interpolation tag 2018/07/25
  character(len=STRING_LEN) :: logstr
  integer :: num_of_25d_data
  integer :: grid_id

  if (index(data_name, "__") > 0) then
    call error("jcup_def_varg", "string __ is not allowed for data_name")
  end if

  if (.not.is_init_conf) then
    call init_data_def()
    is_init_conf = .true.
  end if

  if (.not.is_my_grid(grid_name)) then
    call error("jcup_def_varg", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  grid_id = get_my_grid_num(grid_name)

  if (grid_id==NO_GRID) then
    call error("jcup_def_varg", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  if (present(num_of_data)) then

    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
      logstr = "parameter num_of_data must be <= NUM_OF_EXCHANGE_DATA. num_of_data = " &
               //trim(IntToStr(num_of_data))
      call error("jcup_def_varg", trim(logstr))
    end if

    if (num_of_data > 1) then
      call init_buffer_25d(num_of_data)
    end if
    num_of_25d_data = num_of_data
  else 
    num_of_25d_data = 1
    !write(0,*) "def var_g ", trim(data_name)
  end if

  call def_varg(data_type_ptr, comp_name, data_name, grid_id, num_of_25d_data, &
                send_model_name, send_data_name, recv_mode, &
                interval, time_lag, mapping_tag, exchange_tag, time_intpl_tag)

end subroutine jcup_def_varg

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> finalize data definition
subroutine jcup_end_var_def()
  use jcup_constant, only : ASSYNC_SEND_RECV
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_config, only : exchange_send_config_info, exchange_recv_config_info, &
                          set_configuration, get_comp_exchange_type
  use jcup_data, only : end_def_varp, end_def_varg, check_data_definition, set_exchange_type
  implicit none
  integer :: i, j

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call end_def_varp(i)
      call end_def_varg(i)
    end if
  end do

  do i = 1, get_num_of_total_component()
    call exchange_send_config_info(i)
    call exchange_recv_config_info(i)
  end do

  call set_configuration()
  
  call check_data_definition()

  do i = 1, get_num_of_total_component()
     if (is_my_component(i)) then
        call set_exchange_type(i)
        do j = 1, get_num_of_total_component()
           if (get_comp_exchange_type(i,j) == ASSYNC_SEND_RECV) then
              is_assync_exchange = .true.
           else
              is_sync_exchange = .true.
           end if
        end do
     end if
  end do
  
  is_EndVarDef = .true.

end subroutine jcup_end_var_def

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize coupling time
!> @param[in] time_array array of initial time
! 2014/07/15 [MOD] skip when is_restart == .true.
! 2014/09/02 [MOD] delete call write_grid_mapping_info
! 2014/10/17 [MOD] set_start_time, set_current_time 
subroutine jcup_init_time_int(time_array)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : set_start_time, set_current_time, set_time_data
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_grid, only : write_grid_mapping_info

  implicit none
  integer, intent(IN) :: time_array(6)
  integer :: yyyy, mo, dd, hh, mm
  integer(kind=8) :: ss
  integer :: i

  if (.not.is_Initialized) then
    call jcup_abnormal_end("jcup_InitTime","jcup_Initialize not called")
  end if

  if (is_InitTime) then
    call jcup_abnormal_end("jcup_InitTime","init time double call")
  end if

  !!!!!!!call check_mapping_table_setting()

  !!!!!!call write_grid_mapping_info()


  if (is_restart) then ! 2014/07/15 [ADD]
    call put_log("Jcup_init_time is skipped because jcup_read_restart is called")
    return
  end if

  yyyy = time_array(1) ; mo = time_array(2) ; dd = time_array(3)
  hh = time_array(4) ; mm = time_array(5) ; ss = time_array(6)

  do i = 1, get_num_of_total_component()
    !if (is_my_component(i)) then
      !call set_start_time(i, 1, yyyy, mo, dd, hh, mm, ss)
      !call set_current_time(i, 1, yyyy, mo, dd, hh, mm, ss)
      ss = 0
      call set_start_time(i, 1, 0, 0, 0, 0, 0, ss)
      call set_current_time(i, 1, 0, 0, 0, 0, 0, ss)
    !end if
  end do

  call set_time_data(current_time, 0, 0, 0, 0, 0, int(-1, kind=8))

  is_InitTime = .true.

  call put_log("Time Initialize OK. Start Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
             //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)))

end subroutine jcup_init_time_int

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> mapping table definition
!> @param[in] my_model_name name of my component
!> @param[in] send_model_name name of send component
!> @param[in] send_grid_name name of send grid
!> @param[in] recv_model_name name of recv component
!> @param[in] recv_grid_name name of recv grid
!> @param[in] mapping_tag tag number of this mapping table
!> @param[in] send_grid array of send grid indexes
!> @param[in] recv_grid array of recv grid indexes
! 2016/12/22 [MOD] add call set_pe_num
subroutine jcup_set_mapping_table(my_model_name, &
                                  send_model_name, send_grid_name, recv_model_name,  recv_grid_name, mapping_tag, &
                                  send_grid, recv_grid)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID, MAX_GRID, NO_GRID
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_BcastLocal, jml_SendLeader, jml_RecvLeader, jml_GetMyrank, &
                           jml_GetLeaderRank
  use jcup_utils, only : put_log, IntToStr, error
  use jcup_grid, only : set_grid_mapping_1d, exchange_grid_mapping, &
                        send_grid_mapping, recv_grid_mapping, finish_grid_mapping
  use jcup_grid_base, only : set_pe_num, & ! 2016/12/22
                             get_grid_num, send_index2pe, recv_index2pe, get_grid_min_index, get_grid_max_index
  use jcup_comp, only : get_comp_id_from_name,is_my_component
  use jcup_exchange, only : set_send_grid_tag, set_recv_grid_tag, set_send_mapping_table, set_recv_mapping_table
  implicit none
  character(len=*), intent(IN)  :: my_model_name
  character(len=*), intent(IN)  :: send_model_name, send_grid_name
  character(len=*), intent(IN)  :: recv_model_name, recv_grid_name
  integer, intent(IN)           :: mapping_tag
  integer, intent(IN), optional :: send_grid(:), recv_grid(:)

  integer :: my_model_id, send_model_id, recv_model_id
  integer :: nrx, nry, nsg
  integer :: int_buffer(4)
  integer, allocatable :: send_table(:), recv_table(:)
  logical :: is_my_table
  integer :: map_num, send_grid_num, recv_grid_num
  integer :: i

  call put_log("set mapping table start : "//trim(send_model_name)//":"//trim(recv_model_name) &
              //", grid = "//trim(send_grid_name)//":"//trim(recv_grid_name),1)

  if (.not.is_SetGrid) then
    call jcup_abnormal_end("jcup_set_mapping_table","jcup_SetGrid not called")
  end if

  if (.not.is_EndVarDef) then
    call jcup_abnormal_end("jcup_set_mapping_table","jcup_set_varp, jcup_set_varg, jcup_end_var_def not called")
  end if

  map_num = mapping_tag

  send_grid_num = get_grid_num(send_model_name, send_grid_name)

  if (send_grid_num==NO_GRID) then
    call jcup_abnormal_end("jcup_set_mapping_table", "send model name "//trim(send_model_name)// &
                          " or grid name "//trim(send_grid_name)//" is not defined")
  end if
  
  recv_grid_num = get_grid_num(recv_model_name, recv_grid_name)

  if (recv_grid_num==NO_GRID) then
    call jcup_abnormal_end("jcup_set_mapping_table", "recv model name "//trim(recv_model_name)// &
                          " or grid name "//trim(recv_grid_name)//" is not defined")
  end if

  if (map_num>NUM_OF_EXCHANGE_GRID) then
     call jcup_abnormal_end("set_mapping_table", "mapping_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if
 
  if (send_grid_num>MAX_GRID) then
     call jcup_abnormal_end("set_mapping_table", "send_grid_tag must be <= " &
                            //trim(IntToStr(MAX_GRID)))
  end if
 
  if (recv_grid_num>MAX_GRID) then
     call jcup_abnormal_end("set_mapping_table", "recv_grid_tag must be <= " &
                            //trim(IntToStr(MAX_GRID)))
  end if

 
  my_model_id   = get_comp_id_from_name(trim(my_model_name))
  send_model_id = get_comp_id_from_name(trim(send_model_name))
  recv_model_id = get_comp_id_from_name(trim(recv_model_name))

  ! 2016/12/22
  if (is_my_component(send_model_id)) then
    call set_pe_num(send_model_id, send_grid_name)
  end if
  if (is_my_component(recv_model_id)) then
    call set_pe_num(recv_model_id, recv_grid_name)
  end if


  is_my_table = present(send_grid)

  if (is_my_table) then

    if (jml_isLocalLeader(my_model_id)) then ! 2012/04/12 T.Arakawa [ADD]
      if (minval(send_grid) < get_grid_min_index(send_model_id, send_grid_num)) then
        call error("jcup_set_mapping_table", "send_grid_index < defined grid index, check index")
      end if
      if (maxval(send_grid) > get_grid_max_index(send_model_id, send_grid_num)) then
        call error("jcup_set_mapping_table", "send_grid_index > defined grid index, check index")
      end if

      if (minval(recv_grid) < get_grid_min_index(recv_model_id, recv_grid_num)) then
        call error("jcup_set_mapping_table", "recv_grid_index < defined grid index, check index")
      end if
      if (maxval(recv_grid) > get_grid_max_index(recv_model_id, recv_grid_num)) then
        call error("jcup_set_mapping_table", "recv_grid_index > defined grid index, check index")
      end if
    end if

    nrx = size(send_grid)

  end if


  if (jml_GetLeaderRank(send_model_id) /= jml_GetLeaderRank(recv_model_id)) then
    call send_recv_index2pe()
  end if

 if ((is_my_component(send_model_id)).and.(.not.is_my_component(recv_model_id))) then

    if ((is_my_table).and.(jml_isLocalLeader(send_model_id))) then
      call send_grid_info()
    end if

    call set_send_mapping_table(send_model_id, recv_model_id, map_num)
    call recv_grid_mapping(send_model_id, recv_model_id, map_num)
    call set_send_grid_tag(send_model_id, recv_model_id, map_num, send_grid_num)
  end if    


  if ((is_my_component(recv_model_id)).and.(.not.is_my_component(send_model_id))) then

     if (is_my_table) then
       call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_grid, recv_grid)
      else
        if (jml_isLocalLeader(recv_model_id)) then
          call recv_grid_info()
        else
          nrx = 1 
          allocate(send_table(1), recv_table(1))
        end if
        call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_table, recv_table)
        deallocate(send_table, recv_table)

      end if

    call set_recv_mapping_table(recv_model_id, send_model_id, map_num)
    call send_grid_mapping(send_model_id, recv_model_id, map_num)
    call set_recv_grid_tag(recv_model_id, send_model_id, map_num, recv_grid_num)
  end if

  if (is_my_component(send_model_id).and.(is_my_component(recv_model_id))) then

    if (.not.is_my_table) return

    if (jml_GetLeaderRank(send_model_id)==jml_GetLeaderRank(recv_model_id)) then
      call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_grid, recv_grid)
    else
      if (my_model_id==recv_model_id) then
        call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_grid, recv_grid)
      else
        if (jml_isLocalLeader(send_model_id)) then
          call send_grid_info()
        end if

        if (jml_isLocalLeader(recv_model_id)) then
          call recv_grid_info()
        else
          allocate(send_table(1), recv_table(1))
        end if  

        call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_table, recv_table)
        deallocate(send_table, recv_table)

      end if

    end if

    call exchange_grid_mapping(send_model_id, recv_model_id, map_num)
    call set_send_grid_tag(send_model_id, recv_model_id, map_num, send_grid_num)
    call set_recv_grid_tag(recv_model_id, send_model_id, map_num, recv_grid_num)

    call set_send_mapping_table(send_model_id, recv_model_id, map_num)
    call set_recv_mapping_table(recv_model_id, send_model_id, map_num)

  end if

  call finish_grid_mapping(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num)

  call check_table_data_mismatch(send_model_id, recv_model_id, send_grid_num, recv_grid_num, map_num)


  call put_log("set mapping table end : "//trim(send_model_name)//":"//trim(recv_model_name) &
              //", table number = "//trim(IntToStr(map_num)) &
              //", grid number = "//trim(IntToStr(send_grid_num))//":"//trim(IntToStr(recv_grid_num)),1)

  is_Initialize_completed = .true.


  return

  contains

!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine send_recv_index2pe()
    implicit none

  ! exchange index2pe 
   if (is_my_component(recv_model_id)) then
     if (jml_isLocalLeader(recv_model_id)) then
       call recv_index2pe(recv_model_id, send_model_id, send_grid_num)
     end if
   end if


   if (is_my_component(send_model_id)) then
     if (jml_isLocalLeader(send_model_id)) then
       call send_index2pe(send_model_id, send_grid_num, recv_model_id, recv_grid_num)
    end if
   end if


  end subroutine send_recv_index2pe

!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine send_grid_info()
    implicit none
      int_buffer(1) = nrx 
      call jml_SendLeader(int_buffer,1,1,recv_model_id-1)
      call jml_SendLeader(send_grid,1,nrx,recv_model_id-1)
      call jml_SendLeader(recv_grid,1,nrx,recv_model_id-1)
  end subroutine send_grid_info

!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine recv_grid_info()
    implicit none
          call jml_RecvLeader(int_buffer,1,1,send_model_id-1)
          nrx = int_buffer(1) 
          allocate(send_table(nrx), recv_table(nrx))
          call jml_RecvLeader(send_table,1,nrx,send_model_id-1)
          call jml_RecvLeader(recv_table,1,nrx,send_model_id-1)
 
  end subroutine recv_grid_info

end subroutine jcup_set_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> mapping table definition
!> @param[in] my_model_name name of my component
!> @param[in] send_model_name name of send component
!> @param[in] send_grid_name name of send grid
!> @param[in] recv_model_name name of recv component
!> @param[in] recv_grid_name name of recv grid
!> @param[in] mapping_tag tag number of this mapping table
!> @param[in] send_grid array of send grid indexes
!> @param[in] recv_grid array of recv grid indexes
! 2016/12/27 [NEW]
subroutine jcup_set_mapping_table_local(my_model_name, &
                                  send_model_name, send_grid_name, recv_model_name,  recv_grid_name, mapping_tag, &
                                  num_of_grid, &
                                  send_grid, send_pe, recv_grid)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID, MAX_GRID, NO_GRID
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_BcastLocal, jml_SendLeader, jml_RecvLeader, jml_GetMyrank, &
                           jml_GetLeaderRank
  use jcup_utils, only : put_log, IntToStr, error
  use jcup_grid, only : set_grid_mapping_1d_local, exchange_grid_mapping, &
                        send_grid_mapping, recv_grid_mapping, finish_grid_mapping
  use jcup_grid_base, only : get_grid_num, get_grid_min_index, get_grid_max_index
  use jcup_comp, only : get_comp_id_from_name,is_my_component
  use jcup_exchange, only : set_send_grid_tag, set_recv_grid_tag, set_send_mapping_table, set_recv_mapping_table
  implicit none
  character(len=*), intent(IN)  :: my_model_name
  character(len=*), intent(IN)  :: send_model_name, send_grid_name
  character(len=*), intent(IN)  :: recv_model_name, recv_grid_name
  integer, intent(IN)           :: mapping_tag
  integer, intent(IN), optional :: num_of_grid
  integer, intent(IN), optional :: send_grid(:), send_pe(:), recv_grid(:)

  integer :: my_model_id, send_model_id, recv_model_id
  integer :: nrx, nry, nsg
  integer :: int_buffer(4)
  integer, allocatable :: send_table(:), recv_table(:)
  logical :: is_my_table
  integer :: map_num, send_grid_num, recv_grid_num
  integer :: i

  call put_log("set mapping table (local) start : "//trim(send_model_name)//":"//trim(recv_model_name) &
              //", grid = "//trim(send_grid_name)//":"//trim(recv_grid_name),1)

  if (.not.is_SetGrid) then
    call jcup_abnormal_end("jcup_set_mapping_table_local","jcup_SetGrid not called")
  end if

  if (.not.is_EndVarDef) then
    call jcup_abnormal_end("jcup_set_mapping_table_local","jcup_set_varp, jcup_set_varg, jcup_end_var_def not called")
  end if

  map_num = mapping_tag

  send_grid_num = get_grid_num(send_model_name, send_grid_name)

  if (send_grid_num==NO_GRID) then
    call jcup_abnormal_end("jcup_set_mapping_table_local", "send model name "//trim(send_model_name)// &
                          " or grid name "//trim(send_grid_name)//" is not defined")
  end if
  
  recv_grid_num = get_grid_num(recv_model_name, recv_grid_name)

  if (recv_grid_num==NO_GRID) then
    call jcup_abnormal_end("jcup_set_mapping_table_local", "recv model name "//trim(recv_model_name)// &
                          " or grid name "//trim(recv_grid_name)//" is not defined")
  end if

  if (map_num>NUM_OF_EXCHANGE_GRID) then
     call jcup_abnormal_end("set_mapping_table_local", "mapping_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if
 
  if (send_grid_num>MAX_GRID) then
     call jcup_abnormal_end("set_mapping_table_local", "send_grid_tag must be <= " &
                            //trim(IntToStr(MAX_GRID)))
  end if
 
  if (recv_grid_num>MAX_GRID) then
     call jcup_abnormal_end("set_mapping_table_local", "recv_grid_tag must be <= " &
                            //trim(IntToStr(MAX_GRID)))
  end if

 
  my_model_id   = get_comp_id_from_name(trim(my_model_name))
  send_model_id = get_comp_id_from_name(trim(send_model_name))
  recv_model_id = get_comp_id_from_name(trim(recv_model_name))

  is_my_table = present(send_grid)

  if (is_my_table) then
    if (.not.is_my_component(recv_model_id)) then
     call jcup_abnormal_end("set_mapping_table_local", "grid index must be set on recv model")
    end if
  end if

  if (.not.is_my_table) then
    if (is_my_component(recv_model_id)) then
      call jcup_abnormal_end("set_mapping_table_local", "grid index must be set on recv model")
    end if
  end if

  if (is_my_table) then

    !if (jml_isLocalLeader(my_model_id)) then ! 2012/04/12 T.Arakawa [ADD]
    !  if (minval(send_grid) < get_grid_min_index(send_model_id, send_grid_num)) then
    !    call error("jcup_set_mapping_table", "send_grid_index < defined grid index, check index")
    !  end if
    !  if (maxval(send_grid) > get_grid_max_index(send_model_id, send_grid_num)) then
    !    call error("jcup_set_mapping_table", "send_grid_index > defined grid index, check index")
    !  end if

    !  if (minval(recv_grid) < get_grid_min_index(recv_model_id, recv_grid_num)) then
    !    call error("jcup_set_mapping_table", "recv_grid_index < defined grid index, check index")
    !  end if
    !  if (maxval(recv_grid) > get_grid_max_index(recv_model_id, recv_grid_num)) then
    !    call error("jcup_set_mapping_table", "recv_grid_index > defined grid index, check index")
    !  end if
    !end if

    if (present(num_of_grid)) then
      nrx = num_of_grid
    else
      nrx = size(send_grid)
    end if

  end if


 if ((is_my_component(send_model_id)).and.(.not.is_my_component(recv_model_id))) then

    call set_send_mapping_table(send_model_id, recv_model_id, map_num)
    call recv_grid_mapping(send_model_id, recv_model_id, map_num)
    call set_send_grid_tag(send_model_id, recv_model_id, map_num, send_grid_num)

  end if    


  if ((is_my_component(recv_model_id)).and.(.not.is_my_component(send_model_id))) then

     call set_grid_mapping_1d_local(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, nrx, &
                                    send_grid, recv_grid, send_pe)

    call set_recv_mapping_table(recv_model_id, send_model_id, map_num)
    call send_grid_mapping(send_model_id, recv_model_id, map_num)
    call set_recv_grid_tag(recv_model_id, send_model_id, map_num, recv_grid_num)

  end if

  if (is_my_component(send_model_id).and.(is_my_component(recv_model_id))) then

    if (.not.is_my_table) return

    if (jml_GetLeaderRank(send_model_id)==jml_GetLeaderRank(recv_model_id)) then
       call set_grid_mapping_1d_local(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, nrx, &
                                      send_grid, recv_grid, send_pe)
    else
      if (my_model_id==recv_model_id) then
         call set_grid_mapping_1d_local(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, nrx, &
                                        send_grid, recv_grid, send_pe)
      else
        call jcup_abnormal_end("set_mapping_table_local", "grid index must be set on local leader PE")
      end if

    end if

    call exchange_grid_mapping(send_model_id, recv_model_id, map_num)
    call set_send_grid_tag(send_model_id, recv_model_id, map_num, send_grid_num)
    call set_recv_grid_tag(recv_model_id, send_model_id, map_num, recv_grid_num)

    call set_send_mapping_table(send_model_id, recv_model_id, map_num)

    call set_send_mapping_table(send_model_id, recv_model_id, map_num)
    call set_recv_mapping_table(recv_model_id, send_model_id, map_num)

  end if

  call finish_grid_mapping(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num)

  call check_table_data_mismatch(send_model_id, recv_model_id, send_grid_num, recv_grid_num, map_num)

  call put_log("set mapping table (local) end : "//trim(send_model_name)//":"//trim(recv_model_name) &
              //", table number = "//trim(IntToStr(map_num)) &
              //", grid number = "//trim(IntToStr(send_grid_num))//":"//trim(IntToStr(recv_grid_num)),1)

  is_Initialize_completed = .true.

  return

end subroutine jcup_set_mapping_table_local



!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_mapping_table(mapping_table_checker, model_num, grid_num)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  use jcup_utils, only : IntToStr
  implicit none
  integer, intent(INOUT) :: mapping_table_checker(:)
  integer, intent(IN) :: model_num, grid_num


  return

  if (grid_num>NUM_OF_EXCHANGE_GRID) then
     call jcup_abnormal_end("check_mapping_table", "grid_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if

  if (mapping_table_checker(model_num)<grid_num) then
     call jcup_abnormal_end("check_mapping_table", "mapping table check err, model:" &
                            //trim(IntToStr(model_num))//", index:"//trim(IntToStr(grid_num)))
  end if

end subroutine check_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_table_data_mismatch(send_comp_id, recv_comp_id, &
                                     send_grid_id, recv_grid_id, &
                                     mapping_tag)
  use jcup_utils, only : error
  use jcup_config, only : send_data_conf_type, get_num_of_send_data, get_send_data_conf_ptr_from_id, &
                          recv_data_conf_type, get_num_of_recv_data, get_recv_data_conf_ptr_from_id
  use jcup_comp, only : is_my_component
  implicit none
  integer, intent(IN) :: send_comp_id, recv_comp_id
  integer, intent(IN) :: send_grid_id, recv_grid_id
  integer, intent(IN) :: mapping_tag
  type(send_data_conf_type), pointer :: sd
  type(recv_data_conf_type), pointer :: rd
  integer :: i, j
 
  if (is_my_component(send_comp_id)) then
    do i = 1, get_num_of_send_data(send_comp_id)
      sd => get_send_data_conf_ptr_from_id(send_comp_id, i)
      do j = 1, sd%num_of_my_recv_data
        rd => sd%my_recv_conf(j)
        if ((rd%model_id == recv_comp_id).and.(rd%mapping_tag == mapping_tag)) then
          if (sd%grid_id /= send_grid_id) then
            call error("check_table_data_mismatch", "data: "//trim(sd%name)// &
                       ", grid id mismatch!!! Check jcup_set_varp and jcup_set_mapping_table.") 
          end if
        end if
      end do
    end do
  end if

  if (is_my_component(recv_comp_id)) then
    do i = 1, get_num_of_recv_data(recv_comp_id)
      rd => get_recv_data_conf_ptr_from_id(recv_comp_id, i)
      if ((rd%send_model_id == send_comp_id).and.(rd%mapping_tag == mapping_tag)) then
        if (rd%grid_id /= recv_grid_id) then
          call error("check_table_data_mismatch", "data: "//trim(rd%name)// &
                     ", grid id mismatch!!! Check jcup_set_varg and jcup_set_mapping_table.") 
        end if
      end if
    end do
  end if

end subroutine check_table_data_mismatch



!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set current time
!> @param[in] component_name name of component
!> @param[in] time_real real time from start date
!> @param[in] delta_t delta t
!> @param[in] 
!subroutine jcup_set_date_time_real(component_name, time_real, delta_t)
!  use jcup_comp, only : get_comp_id_from_name
!  use jcup_time, only : time_type, get_start_time, TimeToSecond, SecondToTime
!  implicit none
!  character(len=*), intent(IN) :: component_name
!  real(kind=8), intent(IN) :: time_real
!  real(kind=8), intent(IN) :: delta_t
!  type(time_type) :: start_time
!  integer(kind=8) :: time_sec
!  integer :: comp_id
!  integer :: time_array(6)

!  comp_id = get_comp_id_from_name(component_name)
!  call get_start_time(comp_id, 1, start_time)

!  time_sec = TimeToSecond(start_time)
!  time_sec = time_sec + int(time_real)
!  start_time = SecondToTime(time_sec)

!  time_array(1) = start_time%yyyy
!  time_array(2) = start_time%mo
!  time_array(3) = start_time%dd
!  time_array(4) = start_time%hh
!  time_array(5) = start_time%mm
!  time_array(6) = start_time%ss

!  call jcup_set_date_time_int(component_name, time_array, int(delta_t))

!end subroutine jcup_set_date_time_real

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set current time
!> @param[in] component_name name of component
!> @param[in] time_array array of current time
!> @param[in] delta_t delta t
!> @param[in] 
! 2014/07/03 [MOD] time_array(6) -> time_array(:)
! 2014/10/22 [MOD]
! 2014/10/30 [MOD] integer :: ss -> integer(kind=8) :: ss
! 2014/11/19 [MOD] if (current_time == time) ->  if (current_time >= time)
! 2014/12/08 [ADD] is_exchange_data
subroutine jcup_set_date_time_int(component_name, time_array, delta_t, is_exchange)
  use jcup_constant, only : ADVANCE_SEND_RECV, BEHIND_SEND_RECV
  use jcup_utils, only : put_log, LongIntToStr, IntToStr
  use jcup_time, only : set_current_time, get_current_time, get_before_time, time_type, set_time_data, operator(==), &
                        TU_SEC, TU_MIL, TU_MCR, get_time_unit, inc_time, operator(<), operator(>=)
  use jcup_buffer, only : remove_past_send_data, remove_past_recv_data
  use jcup_comp, only : get_comp_id_from_name, get_num_of_total_component, is_my_component, get_component_name
  use jcup_config, only : set_current_conf, get_comp_exchange_type
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_exchange, only : set_exchange_comp_id, jcup_exchange_data_parallel, jcup_exchange_data_serial
  use jal_api, only : jal_set_time
  implicit none
  character(len=*), intent(IN) :: component_name
  integer, intent(IN) :: time_array(:) ! 2014/07/03 [MOD]
  integer, intent(IN) :: delta_t
  logical, optional :: is_exchange
  integer :: yyyy, mo, dd, hh, mm
  integer(kind=8) :: ss
  integer :: milli_sec, micro_sec
  type(time_type) :: time
  logical :: is_exchange_data
  integer :: comp_id
  integer :: comp
  integer :: my_rank

  call put_log("------------------------------------------------------------------------------------")
  call put_log("--------------------------------- jcup_set_time ------------------------------------")
  call put_log("------------------------------------------------------------------------------------")

  if (is_assync_exchange) then        ! if my binary has sync exchange
    call jal_set_time(delta_t)
    if (.not.is_sync_exchange) return ! if my binary has no sync exchange
  end if
  
  ! check time array and time unit
  if ((get_time_unit() == TU_SEC).and.(size(time_array) < 6)) then
    call jcup_error("jcup_set_date_time", "time unit is SEC and size of time array < 6")
  end if
  if ((get_time_unit() == TU_MIL).and.(size(time_array) < 7)) then
    call jcup_error("jcup_set_date_time", "time unit is MIL and size of time array < 7")
  end if
  if ((get_time_unit() == TU_MCR).and.(size(time_array) < 8)) then
    call jcup_error("jcup_set_date_time", "time unit is MCR and size of time array < 8")
  end if

  yyyy = time_array(1) ; mo = time_array(2) ; dd = time_array(3)
  hh = time_array(4) ; mm = time_array(5) ; ss = time_array(6)
  milli_sec = 0
  micro_sec = 0

  select case (get_time_unit())
  case(TU_SEC)
    call put_log("Set Current Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
               //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)) &
               // ", Delta T : "//trim(IntToStr(delta_t))//", component : "//trim(component_name))
  case(TU_MIL)
    milli_sec = time_array(7)
    if (milli_sec >= 1000) call jcup_error("jcup_set_date_tiem ", " milli second overflow")
    call put_log("Set Current Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
               //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss))//","//trim(IntToStr(milli_sec)) &
               // ", Delta T (milli sec) : "//trim(IntToStr(delta_t))//", component : "//trim(component_name))
  case(TU_MCR)
    milli_sec = time_array(7)
    if (milli_sec >= 1000) call jcup_error("jcup_set_date_tiem ", " milli second overflow")
    micro_sec = time_array(8)
    if (micro_sec >= 1000) call jcup_error("jcup_set_date_tiem ", " micro second overflow")

    call put_log("Set Current Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
               //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss))//","//trim(IntToStr(milli_sec)) &
               //","//trim(IntToStr(micro_sec)) &
               // ", Delta T (micro sec) : "//trim(IntToStr(delta_t))//", component : "//trim(component_name))
  end select
    
  comp_id = get_comp_id_from_name(component_name)

  call set_current_conf(comp_id) 
  call set_exchange_comp_id(comp_id)

  call get_current_time(comp_id, 1, time)

  ! time comparizon
  call put_log("------------------------------- time comparizon -------------------------------")
  call put_log("Component Time : "//trim(IntToStr(time%yyyy))//"/"//trim(IntToStr(time%mo))//"/"//trim(IntToStr(time%dd)) &
               //"/"//trim(IntToStr(time%hh))//"/"//trim(IntToStr(time%mm))//"/"//trim(IntToStr(time%ss)) &
               // ", Delta T : "//trim(IntToStr(delta_t))//", component : "//trim(component_name))
  call put_log("Current   Time : "//trim(IntToStr(current_time%yyyy))//"/"//trim(IntToStr(current_time%mo))//"/"&
               //trim(IntToStr(current_time%dd)) &
               //"/"//trim(IntToStr(current_time%hh))//"/"//trim(IntToStr(current_time%mm))//"/"//trim(IntToStr(current_time%ss)))
  call put_log("-------------------------------------------------------------------------------")


  is_exchange_data = .true.
  if (present(is_exchange)) is_exchange_data = is_exchange


  if (current_time >= time) then ! 2014/11/19 [MOD]
    call put_log("Same Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
             //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)) &
             // " has been set. PARALLEL SEND RECV skipped")
  else
    if (is_exchange_data) then
      call jcup_exchange_data_parallel()
    end if
  end if

  call set_current_conf(comp_id) 
  call set_exchange_comp_id(comp_id)

  if (is_exchange_data) then 
    my_rank = jcup_get_myrank_global()
    do comp = 1, get_num_of_total_component()
      if (comp_id == comp) cycle
      if ((get_comp_exchange_type(comp_id, comp) == ADVANCE_SEND_RECV).or. &
          (get_comp_exchange_type(comp_id, comp) == BEHIND_SEND_RECV)) then
        !!!call jcup_exchange_data_serial_old(comp_id, comp)
        call jcup_exchange_data_serial(comp_id) ! when my model has serial exchange component 
        exit ! exit do loop
      end if
    end do
  end if

  call remove_past_recv_data(time, comp_id)
  call get_before_time(comp_id, 1, time)
  call remove_past_send_data(time, comp_id)

  ! 2014/10/17 [ADD]
  call get_current_time(comp_id, 1, time)

  if (current_time < time) then
    call set_time_data(current_time, time%yyyy, time%mo, time%dd, time%hh, time%mm, time%ss, time%milli_sec, time%micro_sec)
  end if

  call inc_time(time, delta_t)

  yyyy = time%yyyy ; mo = time%mo ; dd = time%dd
  hh   = time%hh   ; mm = time%mm ; ss = time%ss
  milli_sec = time%milli_sec ; micro_sec = time%micro_sec
  ! 2014/10/17

  !do comp = 1, get_num_of_total_component() ! set current time to all my component
  !  if (comp_id == comp) cycle ! skip to set my component time to avoid double setting
  !  if (is_my_component(comp)) then
  !    if (jcup_is_set_time(comp, time_array)) then ! 
  !      call put_log("set current time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
  !                   //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)) &
  !                   //", component : "//trim(get_component_name(comp)))
  !      call set_current_time(comp, 1, yyyy, mo, dd, hh, mm, ss, milli_sec, micro_sec)
  !    end if
  !  end if
  !end do

  call set_current_time(comp_id, 1, yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec, delta_t=delta_t) ! set time and delta t of current component

end subroutine jcup_set_date_time_int

!=======+=========+=========+=========+=========+=========+=========+=========+
! calculate whether current_time + delta_t == itime
! 2014/06/03 [MOD] itime(6) -> itime(:)
! 2014/10/30 [MOD] comment out this subroutine
!!logical function jcup_is_set_time(comp_id, itime)
!!  use jcup_mpi_lib, only : jml_GetMyrankGlobal
!  use jcup_time, only : operator(==), time_type, set_time_data, get_current_time, get_delta_t, inc_time, TU_SEC, TU_MIL, TU_MCR, get_time_unit 
!  implicit none
!  integer, intent(IN) :: comp_id
!  integer, intent(IN) :: itime(:)
!  type(time_type) :: c_time
!  integer         :: delta_t
!  type(time_type) :: n_time
!  integer :: milli_sec, micro_sec  
!  integer(kind=8) :: c_time_sec, n_time_sec
!  integer :: c_time_milli_sec, n_time_milli_sec
!
!  call get_current_time(comp_id, 1, c_time)
!  call get_delta_t(comp_id, 1, delta_t)
!
!  select case (get_time_unit())
!  case(TU_SEC)
!    call inc_time(c_time, delta_t)
!    call set_time_data(n_time, itime(1), itime(2), itime(3), itime(4), itime(5), itime(6))
!    jcup_is_set_time = ( c_time == n_time)
!  case(TU_MIL)
!    call inc_time(c_time, delta_t)
!    call set_time_data(n_time, itime(1), itime(2), itime(3), itime(4), itime(5), itime(6), itime(7))
!    jcup_is_set_time = ( c_time == n_time)
!  case(TU_MCR)
!    call inc_time(c_time, delta_t)
!    call set_time_data(n_time, itime(1), itime(2), itime(3), itime(4), itime(5), itime(6), itime(7), itime(8))
!    jcup_is_set_time = ( c_time == n_time)
!  case default
!    call jcup_error("jcup_is_set_tiem", "time unit parameter error")
!  end select
!
!end function jcup_is_set_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> increment current time
!> @param[in] component_name name of component
!> @param[in] itime increment time
! 2014/07/09 [MOD] itime(6) -> itime(:)
! 2014/10/22 [MOD] delte call inc_time
subroutine jcup_inc_time(component_name, itime)
  use jcup_time, only : get_current_time, time_type, get_delta_t, &
                        inc_time, get_time_unit, TU_SEC, TU_MIL, TU_MCR
  use jcup_comp, only : get_comp_id_from_name
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: component_name
  integer, intent(INOUT) :: itime(:)
  type(time_type) :: time
  integer(kind=8) :: time_sec
  integer :: del_t
  integer :: comp_id

  comp_id = get_comp_id_from_name(component_name)

  call get_current_time(comp_id, 1, time)
  call get_delta_t(comp_id, 1, del_t)

  !write(0,*) "jcup_inc_time 1 "//trim(component_name)//", ",itime

  !call inc_time(time, del_t)

  itime(1) = time%yyyy
  itime(2) = time%mo
  itime(3) = time%dd
  itime(4) = time%hh
  itime(5) = time%mm
  itime(6) = time%ss

  select case(get_time_unit())
  case(TU_SEC)
  case(TU_MIL)
    if (size(itime) < 7) call error("jcup_inc_time", "array size of itime must be >= 7")
    itime(7) = time%milli_sec
  case(TU_MCR)
    if (size(itime) < 8) call error("jcup_inc_time", "array size of itime must be >= 8")
    itime(7) = time%milli_sec
    itime(8) = time%micro_sec
  case default
    call error("jcup_inc_time", "time unit parameter error")
  end select

  !write(0,*) "jcup_inc_time 2 "//trim(component_name)//", ",itime

end subroutine jcup_inc_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> increment calendar time
!> @param[inout] itime increment time
!! @param[in] delta_t delta t
! 2014/11/13 [MOD] (component_name, itime) -> (itime, delta_t)
subroutine jcup_inc_calendar(itime, del_t)
  use jcup_time, only : get_current_time, time_type, get_delta_t, &
                        inc_calendar, get_time_unit, TU_SEC, TU_MIL, TU_MCR
  use jcup_comp, only : get_comp_id_from_name
  use jcup_utils, only : error
  implicit none
  integer, intent(INOUT) :: itime(:)
  integer, intent(IN) :: del_t
  type(time_type) :: time
  integer(kind=8) :: time_sec

  !write(0,*) "jcup_inc_time 1 "//trim(component_name)//", ",itime
  time%yyyy = itime(1)
  time%mo   = itime(2)
  time%dd   = itime(3)
  time%hh   = itime(4)
  time%mm   = itime(5)
  time%ss   = itime(6)
  
  select case(get_time_unit())
  case(TU_SEC)
  case(TU_MIL)
    if (size(itime) < 7) call error("jcup_inc_time", "array size of itime must be >= 7")
    time%milli_sec = itime(7)
  case(TU_MCR)
    if (size(itime) < 8) call error("jcup_inc_time", "array size of itime must be >= 8")
    time%milli_sec = itime(7)
    time%micro_sec = itime(8)
  case default
    call error("jcup_inc_time", "time unit parameter error")
  end select

  call inc_calendar(time, del_t)

  itime(1) = time%yyyy
  itime(2) = time%mo
  itime(3) = time%dd
  itime(4) = time%hh
  itime(5) = time%mm
  itime(6) = time%ss

  select case(get_time_unit())
  case(TU_SEC)
  case(TU_MIL)
    if (size(itime) < 7) call error("jcup_inc_time", "array size of itime must be >= 7")
    itime(7) = time%milli_sec
  case(TU_MCR)
    if (size(itime) < 8) call error("jcup_inc_time", "array size of itime must be >= 8")
    itime(7) = time%milli_sec
    itime(8) = time%micro_sec
  case default
    call error("jcup_inc_time", "time unit parameter error")
  end select

  !write(0,*) "jcup_inc_time 2 "//trim(component_name)//", ",itime

end subroutine jcup_inc_calendar

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_abnormal_end(routine_name, message)
  use jcup_utils, only : error, put_log
  implicit none
  character(len=*),intent(IN) :: routine_name, message

  call put_log("!!! abnormal termination, jc_AbnormalEnd called", 1)

  call error(trim(routine_name),trim(message))

end subroutine jcup_abnormal_end

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_terminate_send_recv(routine_name, message)
  implicit none
  character(len=*), intent(IN) :: routine_name, message

  call jcup_abnormal_end(trim(routine_name),trim(message))

end subroutine jcup_terminate_send_recv

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 2018/07/31 [NEW]

subroutine jcup_put_data_1d(data_type, data, data_vector)
  use jcup_data, only : varp_type
  use jcup_exchange, only : jcup_put_data_1d_double
  use jal_api, only : jal_put_data
  implicit none
  type(varp_type), pointer :: data_type
  real(kind=8), intent(IN) :: data(:)
  real(kind=8), optional, intent(IN) :: data_vector(:)

  if (data_type%is_assync_exchange) then
     call jal_put_data(data_type, data)
  end if

  if (data_type%is_sync_exchange) then
     call jcup_put_data_1d_double(data_type, data, data_vector)
  end if
  
end subroutine jcup_put_data_1d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 2018/07/31 [NEW]

subroutine jcup_put_data_25d(data_type, data, data_vector)
  use jcup_data, only : varp_type
  use jcup_exchange, only : jcup_put_data_25d_double
  use jal_api, only : jal_put_data
  implicit none
  type(varp_type), pointer :: data_type
  real(kind=8), intent(IN) :: data(:,:)
  real(kind=8), optional, intent(IN) :: data_vector(:)

  if (data_type%is_assync_exchange) then
     call jal_put_data(data_type, data)
  end if

  if (data_type%is_sync_exchange) then
     call jcup_put_data_25d_double(data_type, data, data_vector)
  end if
  
end subroutine jcup_put_data_25d


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 2018/07/31 [NEW]

subroutine jcup_get_data_1d(data_type, data, data_vector, is_recv_ok)
  use jcup_constant, only : ASSYNC_SEND_RECV
  use jcup_data, only : varg_type
  use jal_api, only : jal_get_data
  implicit none
  type(varg_type), pointer :: data_type
  real(kind=8), intent(INOUT) :: data(:)
  real(kind=8), intent(OUT), optional :: data_vector(:)
  logical, intent(OUT), optional :: is_recv_ok

  if (data_type%time_lag == ASSYNC_SEND_RECV) then
     call jal_get_data(data_type, data)
  else
     call jcup_get_data_1d_double(data_type, data, data_vector, is_recv_ok)
  end if
  
end subroutine jcup_get_data_1d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 2018/07/31 [NEW]

subroutine jcup_get_data_25d(data_type, data, data_vector, is_recv_ok)
  use jcup_constant, only : ASSYNC_SEND_RECV
  use jcup_data, only : varg_type
  use jal_api, only : jal_get_data
  implicit none
  type(varg_type), pointer :: data_type
  real(kind=8), intent(INOUT) :: data(:,:)
  real(kind=8), intent(OUT), optional :: data_vector(:)
  logical, intent(OUT), optional :: is_recv_ok

  if (data_type%time_lag == ASSYNC_SEND_RECV) then
     call jal_get_data(data_type, data)
  else
     call jcup_get_data_25d_double(data_type, data, data_vector, is_recv_ok)
  end if
  
end subroutine jcup_get_data_25d

#ifndef NO_F2003
!=======+=========+=========+=========+=========+=========+=========+=========+
!> put 1d or 2d or 3d value
!> @param[in] data_type send data type
!> @param[in] data array of send data
! 2015/02/23 [NEW]
! 2015/04/03 [ADD] add data_scalar
subroutine jcup_put_value(data_type, data, data_scalar)
  use jcup_data, only : varp_type, get_varp_data_dim, get_varp_num_of_data
  implicit none
  type(varp_type), pointer :: data_type
  real(kind=8), target, intent(IN) :: data(*)
  real(kind=8), optional, intent(IN) :: data_scalar
  real(kind=8), pointer :: ptr1d(:), ptr2d(:,:)
  real(kind=8) :: data_array(1)
  integer :: i

  if (get_varp_data_dim(data_type) == DATA_25D) then
    ptr2d(1:data_type%my_grid%num_of_point, 1:get_varp_num_of_data(data_type)) => &
                                        data(1:data_type%my_grid%num_of_point*get_varp_num_of_data(data_type))
    if (present(data_scalar)) then
       data_array(1) = data_scalar
       call jcup_put_data_25d_double(data_type, ptr2d, data_array)
    else
      call jcup_put_data_25d_double(data_type, ptr2d)
    end if
  else
    ptr1d(1:data_type%my_grid%num_of_point) => data(1:data_type%my_grid%num_of_point)
    if (present(data_scalar)) then
       data_array(1) = data_scalar
       call jcup_put_data_1d_double(data_type, ptr1d, data_array)
    else
      call jcup_put_data_1d_double(data_type, ptr1d)
    end if
  end if

end subroutine jcup_put_value

#endif


#ifndef NO_F2003
!=======+=========+=========+=========+=========+=========+=========+=========+
!> get 1d or 2d or 3d value
!> @param[in] data_type recv data type
!> @param[in,out] data array of recv data
! 2015/02/23 [NEW]
! 2015/04/03 [ADD] add data_scalar
subroutine jcup_get_value(data_type, data, data_scalar, is_recv_ok)
  use jcup_data, only : varg_type, get_varg_data_dim, get_varg_num_of_data
  implicit none
  type(varg_type), pointer :: data_type
  real(kind=8), target, intent(INOUT) :: data(*)
  real(kind=8), optional, intent(OUT) :: data_scalar
  logical, optional, intent(OUT) :: is_recv_ok
  real(kind=8), pointer :: ptr1d(:), ptr2d(:,:)
  real(kind=8) :: data_array(1)
  
  if (get_varg_data_dim(data_type) == DATA_25D) then
    ptr2d(1:data_type%my_grid%num_of_point, 1:get_varg_num_of_data(data_type)) => &
                                        data(1:data_type%my_grid%num_of_point*get_varg_num_of_data(data_type))
    call jcup_get_data_25d_double(data_type, ptr2d, DATA_VECTOR = data_array, IS_RECV_OK = is_recv_ok)
    if (present(data_scalar)) data_scalar = data_array(1)
 else
    ptr1d => data(1:data_type%my_grid%num_of_point)
    call jcup_get_data_1d_double(data_type, ptr1d, DATA_VECTOR = data_array, IS_RECV_OK = is_recv_ok)
    if (present(data_scalar)) data_scalar = data_array(1)
 end if

end subroutine jcup_get_value

#endif


!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/14 [MOD] time_array(6) -> time_array(:)
! 2014/07/16 [MOD] 2.5D data
! 2014/12/08 [ADD} add jcup_send_final_step_data
! 2015/04/06 [ADD] add jcup_write_restart_gmean
! 2015/11/25 [MOD] time_array(:) -> optional
subroutine jcup_write_restart(fid, time_array)
  use jcup_utils, only : put_log
  use jcup_buffer, only :  buffer_check_write
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name 
  use jcup_io_base, only : jcup_init_io, jcup_io_create_type, jcup_write_restart_base, jcup_write_restart_gmean
  use jcup_grid_base, only : local_area_type, get_num_of_grid, get_my_local_area_ptr
  use jcup_config, only : get_num_of_send_data, send_data_conf_type, get_send_data_conf_ptr_from_id
  use jcup_exchange, only : send_final_step_data
  implicit none
  integer, intent(IN) :: fid
  integer, optional, intent(IN) :: time_array(:)
  type(local_area_type), pointer :: local_area
  type(send_data_conf_type), pointer :: send_data_ptr
  integer, allocatable :: restart_time(:)
  integer :: i, j

  call put_log("-----------------------------------------------------------------------------------------")
  call put_log("--------------------------------- jcup_write_restart ------------------------------------")
  call put_log("-----------------------------------------------------------------------------------------")

  call send_final_step_data() ! 2014/12/08 [ADD]
  is_final_send = .false.

  call jcup_init_io()

  do i = 1, get_num_of_total_component()

    if (is_my_component(i)) then

      if (present(time_array)) then ! 2015/11/24 [ADD]
        allocate(restart_time(size(time_array)))
        restart_time(:) = time_array(:)
      else
        allocate(restart_time(6))
        restart_time(:) = -1
      end if

      call jcup_write_restart_gmean(fid, i, restart_time)
     
      do j = 1, get_num_of_send_data(i)
        send_data_ptr => get_send_data_conf_ptr_from_id(i, j)
        local_area => get_my_local_area_ptr(i, send_data_ptr%grid_id)
        call jcup_io_create_type(i, local_area%grid_num, send_data_ptr%num_of_data, local_area%grid_index)
      end do
         
      call jcup_write_restart_base(fid, i, restart_time)

      deallocate(restart_time)

    end if
  end do

  !!!call buffer_check_write()

end subroutine jcup_write_restart

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/14 [MOD] time_array(6) -> time_array(:)
! 2014/07/16 [MOD] 2.5D data
! 2015/04/06 [ADD] add jcup_read_restart_gmean
! 2015/11/24 [MOD] time_array(:) -> optional
subroutine jcup_read_restart(fid, time_array)
  use jcup_buffer, only :  buffer_check_write
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name 
  use jcup_io_base, only : jcup_init_io, jcup_io_create_type, jcup_read_restart_base, jcup_read_restart_gmean
  use jcup_grid_base, only : local_area_type, get_num_of_grid, get_my_local_area_ptr
  use jcup_config, only : get_num_of_recv_data, get_num_of_send_data, send_data_conf_type, get_send_data_conf_ptr_from_id
  use jcup_utils, only : put_log  
  use jcup_exchange, only : allocate_recv_flag, set_step_flag
  implicit none
  integer, intent(IN) :: fid
  integer, optional, intent(IN) :: time_array(:)
  type(local_area_type), pointer :: local_area
  type(send_data_conf_type), pointer :: send_data_ptr
  integer, allocatable :: restart_time(:)
  integer :: max_flag_size
  integer :: i, j

  call put_log("--------------------------------- jcup_read_restart ------------------------------------")

  max_flag_size = 0
  do i = 1, get_num_of_total_component()
    max_flag_size = max(max_flag_size, get_num_of_recv_data(i))
  end do

  call allocate_recv_flag(max_flag_size)

  call jcup_init_io()

  do i = 1, get_num_of_total_component()

    if (is_my_component(i)) then

      if (present(time_array)) then ! 2015/11/24 [ADD]
        allocate(restart_time(size(time_array)))
        restart_time(:) = time_array(:)
      else
        allocate(restart_time(6))
        restart_time(:) = -1
      end if

      do j = 1, get_num_of_send_data(i)
        send_data_ptr => get_send_data_conf_ptr_from_id(i, j)
        local_area => get_my_local_area_ptr(i, send_data_ptr%grid_id)
        call jcup_io_create_type(i, local_area%grid_num, send_data_ptr%num_of_data, local_area%grid_index)
      end do
         
      call jcup_read_restart_base(fid, i, restart_time)
      call jcup_read_restart_gmean(fid, i, restart_time)

      deallocate(restart_time)

    end if
  end do

  call buffer_check_write()

  call set_step_flag(.false.)
  is_restart = .true.

end subroutine jcup_read_restart

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_interface
