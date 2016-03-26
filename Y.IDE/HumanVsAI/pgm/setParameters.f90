module setParameters ! �萔�̐ݒ�
  
  implicit none
  
  integer, parameter:: & ! �X�C�b�`�̒�`
       ON =1, & ! �I��
       OFF=0, & ! �I�t
       DIS=-1   ! �Ֆʏo�͂��w�肷��

  integer, parameter:: & ! �{�[�h�T�C�Y�Ǝ萔�̒�`
       BOARD_SIZE=8, & ! �{�[�h�T�C�Y(8x8)
       ! �{�[�h�̎n�܂�i�{�[�h�͎����1�}�X�Â��z�I�ȃ}�X�������Ă���(1, 9)�j 
       BOARD_BGN=2, & 
       BOARD_END=BOARD_SIZE+1, & ! �{�[�h�̏I���
       ! �ő�萔(61)�A�{����60(=64-4)�ł��邪�A�֋X�I��+1�����Ă���
       MAX_TURNS=BOARD_SIZE**2-4+1

  integer, parameter:: & ! �}�X�̏�ԂɊւ����`
       EMPTY= 0, & ! ��
       WHITE=-1, & ! ����
       BLACK= 1, & ! ����
       WALL = 2    ! �ǁi�{�[�h�̎���ɔz�u�����j

  integer, parameter:: & ! ��Ԃɂ��AI�t�F�[�Y�Ɋւ����`
       OPENING_MODE=1, & ! ��΃��[�h
       MIDDLE_MODE =2, & ! ���Ճ��[�h
       WDL_MODE    =3, & ! �K���ǂ݃��[�h�A���������𔻒�
       PERFECT_MODE=4    ! ���S�ǂ݃��[�h�A�ŏI�Ֆʂ̎��΂��ő�ƂȂ�
  
  integer, parameter:: & ! �΂��u���邩�}�X��T�����邽�߂Ɏg�p
       NON        =0, & ! ��
       UPPER      =1, & ! ��
       UPPER_LEFT =2, & ! ����
       LEFT       =3, & ! ��
       LOWER_LEFT =4, & ! ����
       LOWER      =5, & ! ��
       LOWER_RIGHT=6, & ! �E��
       RIGHT      =7, & ! �E
       UPPER_RIGHT=8    ! �E��

  real, parameter:: & ! �ő�l�̒�`
       INFINITY=1.e+4

end module setParameters

!----------------------------------------------------------------

module setTheBookParams ! ��΂̐ݒ�

  implicit none

  integer,parameter:: &
!       THEBOOKWAY=3890062 ! ��ΏW�����΃p�^�[����ǂݍ��ސ�
       THEBOOKWAY=454 ! ��ΏW�����΃p�^�[����ǂݍ��ސ�

end module setTheBookParams

!----------------------------------------------------------------

module setIndexParams ! �C���f�b�N�X�̐ݒ�

  use setParameters, only : &
       BOARD_SIZE

  implicit none

  integer,parameter:: &
       INDEX_NUM=3**BOARD_SIZE, & ! �C���f�b�N�X��(3^8=6561)
       STAGE_NUM=12               ! �p�^�[���]���̃X�e�[�W��

end module setIndexParams
