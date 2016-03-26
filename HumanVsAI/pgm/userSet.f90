module userSet ! ���[�U�[�ݒ�
  
  use setParameters, only : &
       ON, OFF, &
       BLACK, WHITE
  
  implicit none
  
  ! �ǂ݂̐[���Ɋւ���ݒ�
  integer, parameter:: &
       NORMAL_DEPTH = 7, & ! ���Ղ̓ǂ݂̐[�� (default: 7)
       WDL_DEPTH    =16, & ! �Ōォ�牽��ڂ�K���ǂ݂ɂ��邩 (default: 16)
       PERFECT_DEPTH=12, & ! �Ōォ�牽��ڂ����S�ǂ݂ɂ��邩 (default: 12)
       SORT_DEPTH   = 4    ! ���Ղł̃\�[�g���̓ǂ݂̐[�� ( default: 5 < NORNAL_DEPTH)
  
  ! �]���֐��t�@�C���̎w��
  character(len=20), parameter:: &
       EVALUATION_FILE='correct003' ! ��10���������琶�������]���֐��\

  ! �m��΂ƒ���\�萔�̏d�݂Ɋւ���ݒ�
  real, parameter:: &
       WEIGHT_STABLESTONE= 0.300, & ! �m��΂̏d�݌W�� (default: 0.300)
       WEIGHT_MOVABLEDISC= 0.014    ! ����\�萔�̏d�݌W�� (default: 0.014)
                                    ! ����0.000�ɂ��Ă����������������Ȃ��B
                                    ! �f�t�H���g�̒l�����邱�ƂŁA��������7.5�p�[�Z���g�̌��シ��B
  ! �����ՖʁAAI�̐΂̐F�Ɋւ���ݒ�
  integer, parameter:: &
       AUTO_IN=OFF, & ! �����Ֆʂ̗L�� (ON or OFF)
                      ! ON�ɂ����fileIn.dat�ɏ����ꂽ�����Ŏ����i�s����B
       AI_COLOR=WHITE ! AI�̐΂̐F (BLACK�i��U�j or WHITE�i��U�j)
  
  ! ���̑��̐ݒ�
  integer, parameter:: &
       USE_RANDOM=OFF ! �l�Ԃ̎�Ԃ������_��AI�ɔC���� (ON or OFF)

end module userSet
