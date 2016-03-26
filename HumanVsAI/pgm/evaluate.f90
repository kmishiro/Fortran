subroutine eval_WDLSearch(eval) ! �K���ǂ݁i�Q�[���I���Ֆʂɓ����Ă���j

  use setVariables, only : &
       DiscsBLACK, DiscsWHITE, &
       CurrentColor

  implicit none

  real &
       eval

  call countDiscs ! �΂̐��𐔂���

  eval=(DiscsBLACK-DiscsWHITE)*CurrentColor ! ���΂̐������߂�

  ! �]���l��������������������3�p�^�[���ɂ���i�}��肪�����N����悤�ɂ���j
  if(eval > 0) then ! ����
     eval= 1.
  else if(eval < 0) then ! ����
     eval=-1.
  else ! ��������
     eval= 0.
  end if

  return
end subroutine eval_WDLSearch

!----------------------------------------------------------------

subroutine eval_perfectSearch(eval) ! ���S�ǂ݁i�Q�[���I���Ֆʂɓ����Ă���j

  use setVariables, only : &
       DiscsBLACK, DiscsWHITE, &
       CurrentColor

  implicit none

  real &
       eval

  call countDiscs ! �΂̐��𐔂���

  eval=(DiscsBLACK-DiscsWHITE)*CurrentColor ! �P���Ɏ��΂̐���]���l�Ƃ���

  return
end subroutine eval_perfectSearch

!----------------------------------------------------------------

subroutine eval_middleSearch(eval) ! ���Ղ̒T��

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END, &
       BLACK, WHITE, EMPTY, &
       INFINITY
  use setVariables, only : &
       Turns, &
       CurrentColor, &
       DiscsBlack, DiscsWhite
  use setPatternEvalVarias, only : &
       HorizEval, &
       DiagoEval, &
       CorneEval
  use setIndexVarias, only : &
       DiffStableStone, &
       DiffMovableDisc
  use userSet, only : &
       WEIGHT_MOVABLEDISC, &
       WEIGHT_STABLESTONE

  implicit none

  integer, parameter:: &
       numHoriz=4, &  ! �����p�^�[���̉�]�i���j
       numDiago1=2, & ! �΂߃p�^�[��1�̉�]�i���j
       numDiago=4, &  ! �΂߃p�^�[���̉�]�i���j
       numCorne=4     ! �R�[�i�[�p�^�[���̉�]�i���j
  real &
       eval, & ! �ŏI�I�ȕ]���l
       allHorizEval, & ! �����p�^�[���̕]���l���v
       allDiagoEval, & ! �΂߃p�^�[���̕]���l���v
       allCorneEval, & ! �R�[�i�[�p�^�[���̕]���l���v
       allDiffStableStone, & ! �m��΂̍��i�� - ���j
       allDiffMovableDisc    ! ����\�萔�̍��i�� - ���j
  integer &
       n, &
       stage
  integer, dimension(numHoriz):: &
       horiz1PLine, & ! �����p�^�[��1�̃C���f�b�N�X
       horiz2PLine, & ! �����p�^�[��2�̃C���f�b�N�X
       horiz3PLine, & ! �����p�^�[��3�̃C���f�b�N�X
       horiz4PLine    ! �����p�^�[��4�̃C���f�b�N�X
  integer, dimension(numDiago1):: &
       diago1PLine ! �΂߃p�^�[��1�̃C���f�b�N�X
  integer, dimension(numDiago):: &
       diago2PLine, & ! �΂߃p�^�[��1�̃C���f�b�N�X
       diago3PLine, & ! �΂߃p�^�[��2�̃C���f�b�N�X
       diago4PLine, & ! �΂߃p�^�[��3�̃C���f�b�N�X
       diago5PLine    ! �΂߃p�^�[��4�̃C���f�b�N�X
  integer, dimension(numCorne):: &
       cornerPLine ! �R�[�i�[�p�^�[���̃C���f�b�N�X

  eval=0. ! �]���l�̏�����

  ! �����̑S�ł��Œ�]���ɂ��A����̑S�ł��ō��]���ɂ���
  call countDiscs ! �΁i�e�F�j�𐔂���

  if(DiscsBlack.eq.0) then
     if(CurrentColor.eq.BLACK) eval=-INFINITY+1
     if(CurrentColor.eq.WHITE) eval= INFINITY-1
     return
  end if
  if(DiscsWhite.eq.0) then
     if(CurrentColor.eq.BLACK) eval= INFINITY-1
     if(CurrentColor.eq.WHITE) eval=-INFINITY+1
     return
  end if

  ! �p�^�[���]���̂��߂̃C���f�b�N�X���擾
  call getHorizontal1Index(horiz1PLine) ! �����p�^�[��1
  call getHorizontal2Index(horiz2PLine) ! �����p�^�[��2
  call getHorizontal3Index(horiz3PLine) ! �����p�^�[��3
  call getHorizontal4Index(horiz4PLine) ! �����p�^�[��4
  call getDiagonal1Index(diago1PLine)   ! �΂߃p�^�[��1
  call getDiagonal2Index(diago2PLine)   ! �΂߃p�^�[��2
  call getDiagonal3Index(diago3PLine)   ! �΂߃p�^�[��3
  call getDiagonal4Index(diago4PLine)   ! �΂߃p�^�[��4
  call getDiagonal5Index(diago5PLine)   ! �΂߃p�^�[��5
  call getCornerIndex(cornerPLine)      ! �R�[�i�[�p�^�[��

  stage=int((Turns+3)*0.2) ! ���݂ǂ̃t�F�[�Y�ɂ��邩���v�Z
  
  allHorizEval=0. ! �����p�^�[���̕]���l���v�Z
  do n=1, numHoriz ! �����p�^�[���̕]���l��S�đ������킹��
     allHorizEval=allHorizEval &
          +HorizEval(horiz1PLine(n), stage, 1) &
          +HorizEval(horiz2PLine(n), stage, 2) &
          +HorizEval(horiz3PLine(n), stage, 3) &
          +HorizEval(horiz4PLine(n), stage, 4)
  end do
  allHorizEval=allHorizEval*0.0625 ! �ő�l��10�Ƃ���悤�K�i�� (1/16)

  allDiagoEval=0. ! �΂߃p�^�[���̕]���l���v�Z
  do n=1, numDiago1 ! �΂߃p�^�[��1�̕]���l��S�đ������킹��
     allDiagoEval=allDiagoEval &
          +DiagoEval(diago1PLine(n), stage, 1)
  end do
  do n=1, numDiago ! �΂߃p�^�[��2-5�̕]���l��S�đ������킹��
     allDiagoEval=allDiagoEval &
          +DiagoEval(diago2PLine(n), stage, 2) &
          +DiagoEval(diago3PLine(n), stage, 3) &
          +DiagoEval(diago4PLine(n), stage, 4) &
          +DiagoEval(diago5PLine(n), stage, 5)
  end do
  allDiagoEval=allDiagoEval*0.0555556 ! �ő�l��10�Ƃ���悤�K�i�� (1/18)

  allCorneEval=0. ! �R�[�i�[�p�^�[���̕]���l���v�Z
  do n=1, numCorne ! �R�[�i�[�p�^�[���̕]���l��S�đ������킹��
     allCorneEval=allCorneEval &
          +CorneEval(cornerPLine(n), stage)
  end do
  allCorneEval=allCorneEval*0.250 ! �ő�l��10�Ƃ���悤�K�i�� (1/4)

  ! �������킹�A�ő�l��10.0�ɂȂ�悤�ɋK�i��
  eval=(allHorizEval+allDiagoEval+allCorneEval)*0.666667 ! (2/3)


  ! �m��΂̍���]���ɉ�����
  allDiffStableStone=0.
  do n=1, numHoriz ! 4�ӂ̊m��΂̍����v�Z
     allDiffStableStone=allDiffStableStone+DiffStableStone(horiz1PLine(n))
  end do
  eval=eval+WEIGHT_STABLESTONE*allDiffStableStone ! �m��΂̏d�݌W���������ĕ]���l�ɑ������킹��


  ! ����\�萔��]���ɉ�����
  allDiffMovableDisc=0.
  do n=1, numHoriz ! ���������̒���\�萔�̍����v�Z
     allDiffMovableDisc=allDiffMovableDisc &
          +DiffMovableDisc(horiz1PLine(n)) &
          +DiffMovableDisc(horiz2PLine(n)) &
          +DiffMovableDisc(horiz3PLine(n)) &
          +DiffMovableDisc(horiz4PLine(n))
  end do
  do n=1, numDiago1 ! �΂ߕ����̒���\�萔�̍����v�Z�i�������C���j
     allDiffMovableDisc=allDiffMovableDisc &
          +DiffMovableDisc(diago1PLine(n))
  end do
  do n=1, numDiago ! �΂ߕ����̒���\�萔�̍����v�Z
     allDiffMovableDisc=allDiffMovableDisc &
          +DiffMovableDisc(diago2PLine(n)+2187) & ! �Ō�1�}�X��E��B�����Ē���\�萔���C�����Ă���
          +DiffMovableDisc(diago3PLine(n)+2916) & ! �Ō�2�}�X��E��B�����Ē���\�萔���C�����Ă���
          +DiffMovableDisc(diago4PLine(n)+3159) & ! �Ō�3�}�X��E��B�����Ē���\�萔���C�����Ă���
          +DiffMovableDisc(diago5PLine(n)+3240)   ! �Ō�4�}�X��E��B�����Ē���\�萔���C�����Ă���
  end do
  eval=eval+WEIGHT_MOVABLEDISC*allDiffMovableDisc ! ����\�萔�̏d�݌W���������ĕ]���l�ɑ������킹��


  ! ���݂̎�Ԏ��_����̕]���l�ɕϊ�
  eval=eval*CurrentColor ! BLACK -> �~1 .or. WHITE -> �~-1

  return
end subroutine eval_middleSearch
