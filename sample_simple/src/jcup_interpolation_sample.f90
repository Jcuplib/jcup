!=======+=========+=========+=========+=========+=========+=========+=========+

module jcup_interpolation_sample
  implicit none
 
!--------------------------------   public  ----------------------------------!
 
  public :: set_operation_index

!--------------------------------   private  ---------------------------------!

  type operation_index_type
    integer :: num_of_operation  ! num of my interpolation operation
    integer, pointer :: my_operation_index(:)   ! index of my operation 
    integer, pointer :: send_data_index(:) ! local send data index of each operation
    integer, pointer :: recv_data_index(:) ! local recv data index of each operation
    integer, pointer :: recv_coef_index(:) ! local recv coef index of each operation
    integer, pointer :: send_coef_index(:) ! local send coef index of each operation
    real(kind=8), pointer :: coef(:) ! local coef
  end type

  type(operation_index_type) :: coi ! current operation index

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_operation_index(recv_model_name, send_model_name, mapping_tag, coef_g)
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_interface, only : jcup_get_model_id, jcup_get_local_operation_index, &
                             jcup_get_num_of_send_grid, jcup_get_num_of_recv_grid, &
                             jcup_set_local_coef, OPERATION_COEF
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, optional, intent(IN) :: mapping_tag
  real(kind=8), intent(IN) :: coef_g(:) 
  integer :: recv_model_id, send_model_id
  integer :: grid_tag

  if (present(mapping_tag)) then
    grid_tag = mapping_tag
  else
    grid_tag = 1
  end if

  call jcup_get_local_operation_index(recv_model_name, send_model_name, grid_tag, &
                                      coi%num_of_operation, &
                                      coi%my_operation_index, &
                                      coi%send_data_index, &
                                      coi%recv_data_index, &
                                      coi%send_coef_index, &
                                      coi%recv_coef_index)

  allocate(coi%coef(coi%num_of_operation))

  call jcup_set_local_coef(recv_model_name, send_model_name, grid_tag, coef_g, coi%coef, OPERATION_COEF)

end subroutine set_operation_index

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine interpolate_data_sample(recv_model, send_model, send_data, & 
                                   recv_data, num_of_data, mapping_tag, data_tag)
  use jcup_interface, only : jcup_get_comp_num_from_name
  implicit none
  character(len=*), intent(IN) :: recv_model, send_model
  real(kind=8), intent(IN) :: send_data(:,:)
  real(kind=8), intent(INOUT) :: recv_data(:,:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: mapping_tag
  integer, intent(IN) :: data_tag(:)

  integer :: i, d
  integer :: send_point, recv_point

    recv_data(:,:) = 0.d0
    do d = 1, num_of_data
      do i = 1, size(coi%send_data_index)
        send_point = coi%send_data_index(i)
        recv_point = coi%recv_data_index(i) 
        recv_data(recv_point,d) = recv_data(recv_point,d) + send_data(send_point,d)*coi%coef(i)
      end do
    end do

    return

end subroutine interpolate_data_sample

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_interpolation_sample


!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine interpolate_data(recv_model, send_model, mapping_tag, sn1, sn2, send_data, & 
                            rn1, rn2, recv_data, num_of_data, tn, data_tag)
  use jcup_interpolation_sample, only : interpolate_data_sample
  implicit none
  character(len=*), intent(IN) :: recv_model, send_model
  integer, intent(IN) :: mapping_tag
  integer, intent(IN) :: sn1, sn2
  real(kind=8), intent(IN) :: send_data(sn1,sn2)
  integer, intent(IN) :: rn1, rn2
  real(kind=8), intent(INOUT) :: recv_data(rn1,rn2)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: tn
  integer, intent(IN) :: data_tag(tn)

  call interpolate_data_sample(recv_model, send_model, send_data, recv_data, num_of_data, mapping_tag, data_tag)

end subroutine interpolate_data

!=======+=========+=========+=========+=========+=========+=========+=========+
