subroutine move(x, y, correctIn) ! �΂�u���A���Ԃ�

  use setParameters, only : &
       NON, &
       ON, OFF, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MovableDir, &
       MemoryCurrentColor, &
       MemoryRawBoard, &
       MemoryPutPlace, &
       CurrentColor, &
       RawBoard

  implicit none

  integer &
       correctIn
  integer &
       x, y

  if(x < BOARD_BGN .or. x > BOARD_END) then ! x���W���{�[�h�O��������AcorrectIn��OFF�������Ԃ�
     correctIn=OFF
     return
  end if
  if(y < BOARD_BGN .or. y > BOARD_END) then ! y���W���{�[�h�O��������AcorrectIn��OFF�������Ԃ�
     correctIn=OFF
     return
  end if
  if(MovableDir(Turns, x, y) == NON) then ! �w�肵���}�X�ɂ͒u���Ȃ��Ƃ��AcorrectIn��OFF�������Ԃ�
     correctIn=OFF
     return
  end if

  MemoryCurrentColor(Turns)=CurrentColor ! ���݂̎�ԁi�΂̐F�j��ۑ�
  MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END) & ! ���݂̔Ֆʂ�ۑ�
  =RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)

  call flipDiscs(x, y) ! ���ۂɐ΂�Ԃ��T�u���[�`��

  MemoryPutPlace(Turns)=10*x+y ! �΂�u�����ꏊ��2���̐��l�ŕۑ��i��Fa1=22�j

  Turns=Turns+1 ! �΂�u�����Ԃ�����A�^�[����i�߂�

  CurrentColor=-CurrentColor ! ��Ԍ��

  call initMovable(CurrentColor) ! MovableDir�i�u����}�X�̏��j���X�V

  return
end subroutine move

!----------------------------------------------------------------

subroutine pass(correctIn) ! �p�X���\�����ׁA�\�ȏꍇ��correctIn=ON�ɂ��ĕԂ�

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END, &
       NON
  use setVariables, only : &
       CurrentColor, &
       Turns, &
       MovableDir

  implicit none

  integer &
       correctIn
  integer &
       x, y

  call initMovable(CurrentColor) ! MovableDir�i�u����}�X�̏��j���X�V
  do x=BOARD_BGN, BOARD_END
     do y=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= NON) return ! �u����ꏊ����ł�����΁A�p�X�ł͂Ȃ�
     end do
  end do

  correctIn=ON ! �p�X�\�Ɠ����ɓ��͂̐���𐳂ɂ���

  CurrentColor=-CurrentColor ! �p�X�ɂ���Ď�Ԍ��

  call initMovable(CurrentColor) ! MovableDir�i�u����}�X�̏��j���X�V�i���i���j���_���甒�i���j���_�ɂ���j

  return
end subroutine pass

!----------------------------------------------------------------

subroutine unundo(correctIn) ! ������

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END
  use userSet, only : &
       AI_COLOR
  use setVariables, only : &
       CurrentColor, &
       Turns, &
       MemoryCurrentColor, &
       MemoryRawBoard, &
       RawBoard, &
       MemoryPutPlace

  implicit none

  integer &
       correctIn

  do ! �O��̎����̔Ԃ܂Ŗ߂��i1��O�������̔ԂƂ͌���Ȃ����Ƃɒ��Ӂj

     if(Turns <= 1) then ! 1��ڂ܂ł͎��߂��Ȃ�
        correctIn=OFF
        call output('can not undo.', ON) ! �x���̃^�[�~�i���o��
        return
     end if

     MemoryCurrentColor(Turns)=0 ! ���݂̐F����������
     MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=0 ! ���݂̔Ֆʂ�������
     MemoryPutPlace(Turns)=0 ! ���ݑł����}�X����������

     Turns=Turns-1 ! 1��߂��Ă݂�
  
     CurrentColor=MemoryCurrentColor(Turns) ! 1��O�̎�Ԃ̐F���Ăяo��
     RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END) & ! 1��O�̔Ֆʂ��Ăяo��
     =MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)
  
     if(CurrentColor /= AI_COLOR) exit ! �����̔Ԃł���΁A���[�v�𔲂���

  end do

  correctIn=ON ! ���͂͐�����

  call countDiscs ! �΂𐔂�����

  return
end subroutine unundo

!----------------------------------------------------------------

subroutine flipDiscs(x, y) ! �΂�u�����Ԃ�

  use setParameters, only : &
       UPPER, &
       LOWER, &
       LEFT, &
       RIGHT, &
       UPPER_RIGHT, &
       UPPER_LEFT, &
       LOWER_LEFT, &
       LOWER_RIGHT
  use setVariables, only : &
       Turns, &
       RawBoard, &
       CurrentColor, &
       MovableDir
  
  implicit none

  integer &
       x, y, &   ! �΂�u�������W
       xv, yv, & ! x, y���W�̉��ϐ�
       dir       ! x, y�ɐ΂�u�����ꍇ�ǂ̕����ɐ΂��Ԃ邩���L�^����

  RawBoard(x, y)=CurrentColor ! �����ɐ΂�u��

  dir=MovableDir(Turns, x, y) ! �ǂ̕����ɐ΂��Ԃ邩����

  xv=x
  yv=y
  if(btest(dir, UPPER-1)) then ! ������idir��UPPER�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̏オ�����̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1��̔���Ɉڂ�
     do while(RawBoard(xv, yv-1) /= CurrentColor)
        RawBoard(xv, yv-1)=CurrentColor
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER-1)) then ! �������idir��LOWER�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̉��������̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1���̔���Ɉڂ�
     do while(RawBoard(xv, yv+1) /= CurrentColor)
        RawBoard(xv, yv+1)=CurrentColor
        yv=yv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LEFT-1)) then ! �������idir��LEFT�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̍��������̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1���̔���Ɉڂ�
     do while(RawBoard(xv-1, yv) /= CurrentColor)
        RawBoard(xv-1, yv)=CurrentColor
        xv=xv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, RIGHT-1)) then ! �E�����idir��RIGHT�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̉E�������̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1�E�̔���Ɉڂ�
     do while(RawBoard(xv+1, yv) /= CurrentColor)
        RawBoard(xv+1, yv)=CurrentColor
        xv=xv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, UPPER_RIGHT-1)) then ! �E������idir��UPPER_RIGHT�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̉E�オ�����̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1�E��̔���Ɉڂ�
     do while(RawBoard(xv+1, yv-1) /= CurrentColor)
        RawBoard(xv+1, yv-1)=CurrentColor
        xv=xv+1
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, UPPER_LEFT-1)) then ! ��������idir��UPPER_LEFT�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̍��オ�����̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1����̔���Ɉڂ�
     do while(RawBoard(xv-1, yv-1) /= CurrentColor)
        RawBoard(xv-1, yv-1)=CurrentColor
        xv=xv-1
        yv=yv-1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER_LEFT-1)) then ! ���������idir��LOWER_LEFT�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̍����������̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1�����̔���Ɉڂ�
     do while(RawBoard(xv-1, yv+1) /= CurrentColor)
        RawBoard(xv-1, yv+1)=CurrentColor
        xv=xv-1
        yv=yv+1
     end do
  end if

  xv=x
  yv=y
  if(btest(dir, LOWER_RIGHT-1)) then ! �E�������idir��LOWER_RIGHT�Ԗڂ̃r�b�g���P���ǂ����𔻒�j
     ! �u�����ꏊ�̉E���������̐΂ł͂Ȃ��ꍇ�ɁA�����������̐΂ɂ��Ă���1�E���̔���Ɉڂ�
     do while(RawBoard(xv+1, yv+1) /= CurrentColor)
        RawBoard(xv+1, yv+1)=CurrentColor
        xv=xv+1
        yv=yv+1
     end do
  end if

  call countDiscs ! �΁i�F�j�̐��𐔂���

  return
end subroutine flipDiscs

!----------------------------------------------------------------

subroutine isGameOver(gameRetire, gameContinue) ! �Q�[���I�������s���𔻒f

  use setParameters, only : &
       ON, OFF, &
       EMPTY, &
       MAX_TURNS, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MovableDir, &
       CurrentColor

  implicit none

  integer &
       gameRetire, &
       gameContinue ! ���̃T�u���[�`���Ō��肷��ϐ�
  integer &
       x, y

  ! �ŏI�^�[���������̓Q�[�����~�R�}���h�����͂���Ă���΁AgameContinue��OFF�ɂ��ĕԂ�
  if(Turns == MAX_TURNS .or. gameRetire == ON) then 
     gameContinue=OFF
     return
  end if

  ! �������u����ꏊ�����邩�𒲂ׂ�
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= EMPTY) then
           gameContinue=ON ! �u����ꏊ������΃Q�[�����s
           return
        end if
     end do
  end do

  call initMovable(-CurrentColor) ! �u����}�X���𑊎�ɂ���

  ! ���肪�u����ꏊ�����邩�𒲂ׂ�
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= EMPTY) then
           gameContinue=ON ! �u����ꏊ������΃Q�[�����s
           call initMovable(CurrentColor) ! �u����}�X���������ɖ߂��Ă���
           return
        end if
     end do
  end do

  gameContinue=OFF ! �����܂ł���΂ǂ����������ꏊ���Ȃ��Ƃ������ƁA�Q�[���I��

  return
end subroutine isGameOver

!----------------------------------------------------------------

subroutine retireGame(correctIn, gameRetire) ! �Q�[���~�߂�

  use setParameters, only : &
       ON

  implicit none

  integer &
       correctIn, &
       gameRetire

  correctIn=ON  ! ���͂𐳂�
  gameRetire=ON ! �Q�[�����~��ON��

  return
end subroutine retireGame

!----------------------------------------------------------------

subroutine initMovable(turgetColor) ! checkMobility���Ăяo���A�u����ꏊ��T��

  use setParameters, only : &
       BOARD_BGN, BOARD_END

  implicit none
  
  integer &
       turgetColor ! BLACK��WHITE�������Ă���A���̐F��ΏۂƂ�������ƂȂ�
  
  integer &
       x, y
  
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END

        ! turgetColor�̐F�ɂ��āA���W(x, y)�ɐ΂�u�����ꍇ�ǂ̕����ɐ΂�Ԃ��邩�̏����擾����
        call checkMobility(turgetColor, x, y) 

     end do
  end do

  return
end subroutine initMovable

!----------------------------------------------------------------

subroutine checkMobility(turgetColor, x, y) 
! turgetColor�̐F�ɂ��āA���W(x, y)�ɐ΂�u�����ꍇ�ǂ̕����ɐ΂�Ԃ��邩�̏����擾����

  use setParameters, only : &
       NON, &
       EMPTY, &
       UPPER, &
       LOWER, &
       LEFT, &
       RIGHT, &
       UPPER_RIGHT, &
       UPPER_LEFT, &
       LOWER_LEFT, &
       LOWER_RIGHT
  use setVariables, only : &
       RawBoard, &
       MovableDir, &
       Turns

  implicit none

  integer &
       turgetColor
  integer &
       x, y, &
       xv, yv
  integer &
       dir
  
  if(RawBoard(x, y) /= EMPTY) then ! ���̏ꏊ�Ɋ��ɐ΂�����΁A�u���Ȃ�
     MovableDir(Turns, x, y)=NON
     return
  end if
  
  dir=NON ! dir�̏�����
  
  xv=x
  yv=y
  ! ������ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv, yv-1) == -turgetColor) then ! 1�オ����̐�
     xv=xv
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2�オ����̐΂̂Ƃ��́A����1���
        yv=yv-1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER-1) ! dir��UPPER�Ԗڂ̃r�b�g��1�ɐݒ�
  end if

  xv=x
  yv=y
  ! �������ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv, yv+1) == -turgetColor) then ! 1��������̐�
     xv=xv
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2��������̐΂̂Ƃ��́A����1����
        yv=yv+1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER-1) ! dir��LOWER�Ԗڂ̃r�b�g��1�ɐݒ�
  end if
  
  xv=x
  yv=y
  ! �������ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv-1, yv) == -turgetColor) then ! 1��������̐�
     xv=xv-2
     yv=yv
     do while(RawBoard(xv, yv) == -turgetColor) ! 2��������̐΂̂Ƃ��́A����1����
        xv=xv-1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LEFT-1) ! dir��LEFT�Ԗڂ̃r�b�g��1�ɐݒ�
  end if
  
  xv=x
  yv=y
  ! �E�����ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv+1, yv) == -turgetColor) then ! 1�E������̐�
     xv=xv+2
     yv=yv
     do while(RawBoard(xv, yv) == -turgetColor) ! 2�E������̐΂̂Ƃ��́A����1�E��
        xv=xv+1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, RIGHT-1) ! dir��RIGHT�Ԗڂ̃r�b�g��1�ɐݒ�
  end if

  xv=x
  yv=y
  ! �E������ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv+1, yv-1) == -turgetColor) then ! 1�E�オ����̐�
     xv=xv+2
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2�E�オ����̐΂̂Ƃ��́A����1�E���
        xv=xv+1
        yv=yv-1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER_RIGHT-1) ! dir��UPPER_RIGHT�Ԗڂ̃r�b�g��1�ɐݒ�
  end if
  
  xv=x
  yv=y
  ! ��������ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv-1, yv-1) == -turgetColor) then ! 1���オ����̐�
     xv=xv-2
     yv=yv-2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2���オ����̐΂̂Ƃ��́A����1�����
        xv=xv-1
        yv=yv-1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, UPPER_LEFT-1) ! dir��UPPER_LEFT�Ԗڂ̃r�b�g��1�ɐݒ�
  end if

  xv=x
  yv=y
  ! ���������ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv-1, yv+1) == -turgetColor) then ! 1����������̐�
     xv=xv-2
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2����������̐΂̂Ƃ��́A����1������
        xv=xv-1
        yv=yv+1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER_LEFT-1) ! dir��LOWER_LEFT�Ԗڂ̃r�b�g��1�ɐݒ�
  end if
  
  xv=x
  yv=y
  ! �E�������ɂ��āA�΂�Ԃ��邩�𒲂ׂ�
  if(RawBoard(xv+1, yv+1) == -turgetColor) then ! 1�E��������̐�
     xv=xv+2
     yv=yv+2
     do while(RawBoard(xv, yv) == -turgetColor) ! 2�E��������̐΂̂Ƃ��́A����1�E����
        xv=xv+1
        yv=yv+1
     end do
     ! �ŏI�I�Ɏ����̐΂�����΁A���̕����ɂ͐΂�Ԃ���
     if(RawBoard(xv, yv) == turgetColor) dir=ibset(dir, LOWER_RIGHT-1) ! dir��LOWER_RIGHT�Ԗڂ̃r�b�g��1�ɐݒ�
  end if

  MovableDir(Turns, x, y)=dir ! �ŏI�I��dir�ɂ��ׂĂ̕Ԃ�������ɂ��Ă̏�񂪓����Ă���
  
  return
end subroutine checkMobility

!----------------------------------------------------------------

subroutine countDiscs ! �Տ�̐�(�e�F)�̐��𐔂���

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BLACK, WHITE
  use setVariables, only : &
       DiscsBLACK, DiscsWHITE, DiscsEMPTY, &
       RawBoard

  implicit none

  integer &
       x, y

  ! �ΐ��̏�����
  DiscsBLACK=0
  DiscsWHITE=0
  DiscsEMPTY=0

  ! �V���v���ɂ��ׂẴ}�X�ɂ��āA���ׂĂ���
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(RawBoard(x, y) == BLACK) then
           DiscsBLACK=DiscsBLACK+1
        else if(RawBoard(x, y) == WHITE) then
           DiscsWHITE=DiscsWHITE+1
        else
           DiscsEMPTY=DiscsEMPTY+1
        end if
     end do
  end do

  return
end subroutine countDiscs

!----------------------------------------------------------------

subroutine random(in) ! �l�Ԃ̎�������_���ɔC����

  use setParameters, only : &
       BOARD_BGN, &
       BOARD_END, &
       NON
  use setVariables, only : &
       Turns, &
       MovableDir

  implicit none

  character(len=2) in
  integer x, y

  integer, allocatable, dimension(:) :: seed ! �����_���֐��̃V�[�h�l
  integer nrand ! �V�[�h�l�z��̑傫��
  integer clock ! ���Ԃ��i�[�A�V�[�h�l�͎��Ԃ̊֐��Ƃ���
  real, dimension(2):: randomNum ! 2�����̃����_����

  call random_seed(size=nrand)   ! �V�[�h�l�z��̃T�C�Y���擾
  allocate(seed(nrand))          ! �V�[�h�l�z��̓��I���t
  call system_clock(count=clock) ! ���Ԃ��擾

  seed=clock ! ���Ԃ��V�[�h�l�Ƃ���
  call random_seed(put=seed)

  do
     call random_number(randomNum) ! 2���������_�������擾
     x=int(randomNum(1)*10)        ! 0-10�̒l�ɕϊ� (x)
     y=int(randomNum(2)*10)        ! 0-10�̒l�ɕϊ� (y)

     ! �擾�������W(x, y)���Տ�ł���A�����ɐ΂�u�����Ƃ��\�ł���΃��[�v�𔲂��������
     if(x.ge.2.and.x.le.9.and.y.ge.2.and.y.le.9) then
        if(MovableDir(Turns, x, y).ne.NON) exit
     end if
  end do

  call writeIn(x, y, in) ! x, y��in�ɕϊ��i��F(2, 2) -> a2�j

  return
end subroutine random

!----------------------------------------------------------------

subroutine writeIn(x, y, in) ! ���͂𕶎����琔�l�ɕϊ��i�� subroutine readIn�j

  use setParameters, only : &
       BOARD_BGN, BOARD_END

  implicit none

  integer, intent(in) :: &
       x, y
  character(len=2), intent(out) :: &
       in
  character(len=1) &
       inx, iny !x, y���ꂼ��̕���

  ! x���W�ɂ���
  select case (x)
  case(2); inx='a'
  case(3); inx='b'
  case(4); inx='c'
  case(5); inx='d'
  case(6); inx='e'
  case(7); inx='f'
  case(8); inx='g'
  case(9); inx='h'
  case default
     write(06, '(a)') 'program bug:: s:rewriteIn(X)'
     stop
  end select

  ! y���W�ɂ���
  if(y < BOARD_BGN .and. y > BOARD_END) then
     write(06, '(a)') 'program bug:: s:rewriteIn(Y)'
     stop
  end if
  write(iny, '(i1)') y-1

  ! ����
  in=inx//iny

  return
end subroutine writeIn

!----------------------------------------------------------------

subroutine readIn(x, y, correctIn, in) ! ���͂𐔒l���當���ɕϊ��i�� subroutine writeIn�j

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END

  implicit none

  integer, intent(out) :: &
       x, y
  integer, intent(out) :: &
       correctIn
  character(len=2), intent(in) :: &
       in
  character(len=1) &
       inx, iny

  ! x���W�ɂ���
  inx=in(1:1)
  select case (inx)
  case('a'); x=2
  case('b'); x=3
  case('c'); x=4
  case('d'); x=5
  case('e'); x=6
  case('f'); x=7
  case('g'); x=8
  case('h'); x=9
  case default
     correctIn=OFF
     return
  end select

  ! y���W�ɂ���
  iny=in(2:2)
  read(iny, '(i1)', err=100) y
  y=y+1
  if(y < BOARD_BGN .and. y > BOARD_END) then
     correctIn=OFF
     return
  end if

  correctIn=ON
  return

100 correctIn=OFF
  return
end subroutine readIn
