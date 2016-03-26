subroutine ai(switchPhase, in, str_eval, phase) ! �l�H�m�\(AI)�̎v�l�𐧌䂷��

  use setParameters, only : &
       MAX_TURNS, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       WDL_MODE, &
       PERFECT_MODE
  use userSet, only : & ! ���[�U�ݒ�
       NORMAL_DEPTH, &  ! ���Փǂ݂̒T���[�x
       WDL_DEPTH, &     ! �K���ǂ݂̒T���[�x
       PERFECT_DEPTH, & ! ���S�ǂ݂̒T���[�x
       SORT_DEPTH       ! �\�[�g�̒T���[�x
  use setVariables, only : &
       Turns

  implicit none

  real &
       eval
  character(len=2) &
       in
  character(len=7) &
       phase
  character(len=8) &
       str_eval
  integer &
       switchPhase, &
       limit, sortLimit

  if(switchPhase == OPENING_MODE) then ! ����(Opening)
     call theBook(in, switchPhase, eval) ! ��΂�I�ԃT�u���[�`�����Ăяo��
     phase='Opening'
     write(str_eval, '(f8.3)') eval
  else ! �t�F�[�Y�̈ڍs
     if(MAX_TURNS-Turns <= PERFECT_DEPTH) then ! ���S�ǂ݂Ɉڍs
        switchPhase=PERFECT_MODE
     else if(MAX_TURNS-Turns <= WDL_DEPTH) then ! �K���ǂ݂Ɉڍs
        switchPhase=WDL_MODE
     else ! ���Փǂ݂Ɉڍs
        switchPhase=MIDDLE_MODE
     end if
  end if

  if(switchPhase == MIDDLE_MODE) then ! ����(Middle)
     limit=NORMAL_DEPTH   ! ���Ղ̒T���[�x����
     sortLimit=SORT_DEPTH ! �\�[�g�T���[�x�̑��
     call treeSearch(in, switchPhase, eval, limit, sortLimit) ! �Q�[���ؒT�����J�n
     phase='Middle'
     write(str_eval, '(f8.3)') eval
  end if

  if(switchPhase == WDL_MODE) then ! �K���ǂ�(WDL)
     limit=MAX_TURNS ! �T���[�x�̓Q�[���I�����܂�
     sortLimit=0     ! �\�[�g�͂��Ȃ�
     call treeSearch(in, switchPhase, eval, limit, sortLimit) ! �Q�[���ؒT�����J�n
     if(eval == 1.) then ! �K�����肪��������
        write(str_eval, '(a)') '     WIN'
     else if(eval == 0.) then ! ���ɍőP��Ői�s���������������
        write(str_eval, '(a)') '    DRAW'
     else ! ���ɍőP��Ői�s�����畉���邱�Ƃ����������̂ŁA���ՒT������蒼��
        switchPhase=MIDDLE_MODE ! �t�F�[�Y�𒆔Ղɖ߂�
        limit=NORMAL_DEPTH      ! �T���[�x�����Ղɖ߂�
        sortLimit=SORT_DEPTH    ! �\�[�g�T���[�x�����Ղɖ߂�
        call treeSearch(in, switchPhase, eval, limit, sortLimit) ! �Q�[���ؒT�����J�n
        write(str_eval, '(a)') '    LOSE'
     end if
     phase='WDL'
  end if

  if(switchPhase == PERFECT_MODE) then ! ���S�ǂ�(Perfect)
     limit=MAX_TURNS ! �T���[�x�̓Q�[���I�����܂�
     sortLimit=0     ! �\�[�g�͂��Ȃ�
     call treeSearch(in, switchPhase, eval, limit, sortLimit) ! �Q�[���ؒT�����J�n
     phase='Perfect'
     write(str_eval, '(f8.3)') eval
  end if

  return
end subroutine ai

!----------------------------------------------------------------

subroutine treeSearch(in, switchPhase, evalMax, limit, sortLimit) ! �Q�[���ؒT��

  use setParameters, only : &
       NON, &
       INFINITY, &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END, &
       MAX_TURNS
  use setVariables, only : &
       MovableDir, &
       Turns
  
  implicit none

  integer &
       switchPhase
  character(len=2) &
       in
  integer &
       x, y
  integer, dimension(MAX_TURNS-1):: &
       xp, yp ! �łĂ��̍��W������
  logical, allocatable:: &
       mask(:) ! �\�[�g�p�̃}�X�N�z��
  integer, allocatable:: &
       xpTmp(:), ypTmp(:) ! �\�[�g�p�̉����W(x, y)
  real, allocatable:: &
       sortEval(:) ! �\�[�g�p�̉��]���l
  integer &
       n, nm, &
       limit, sortLimit, &
       nSort(1)
  real &
       eval, evalMax, &
       alpha, beta ! �A���t�@�E�x�[�^�T���p�̃��ƃ�

  ! ������
  xp(1:MAX_TURNS-1)=0
  yp(1:MAX_TURNS-1)=0

  ! ���ׂĂ̒u����ʒu�𐶐�
  n=0
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= NON) then ! ����(x, y)�ɑłĂ�Ƃ����ɓ���
           n=n+1
           xp(n)=x
           yp(n)=y
        end if
     end do
  end do
  nm=n
    
  ! �u����ꏊ�������Ƃ��̓G���[(�u���Ȃ���Ԃł��̃T�u���[�`���ɓ����ė��Ȃ�)
  if(nm == 0) then
     write(06, '(a)') 'program bug:: s:AI nm=0'
     stop
  end if

  ! �{�i�I�ɒT�����J�n����O�ɐ󂢒T�����s���]���l���������ȏ��ɕ��׊�����i�\�[�g�j
  if(sortLimit /= 0) then ! �\�[�g�T���[�x��0�̂Ƃ��̓\�[�g����
     if(switchPhase /= 2) then ! AI�t�F�[�Y�����Ղ̂Ƃ��̂݃\�[�g�@�\���g�p����
        write(06, '(a)') 'program bug:: s:AI sort'
        write(06, *) switchPhase
        stop
     end if

     ! �\�[�g�p�̔z��̑傫��������A�łĂ��̐��ɂȂ�
     allocate(mask(nm), sortEval(nm), xpTmp(nm), ypTmp(nm))
     mask(1:nm)=.true. ! �}�X�N�ɂ͎n�߂��ׂāu�^�v�����Ă���

     ! ���T���ŕ]���l���擾
     evalMax=-INFINITY ! �]���l�̍ő�l��������
     do n=1, nm

        ! �΂�łiAI�p�Amain�̈�ʗp����]���ȑ������菜���A�C���f�b�N�X�𗘗p���΂𗠕Ԃ��Ă���j
        call AImove(xp(n), yp(n)) 

        ! ���ƃ���������
        alpha=-INFINITY
        beta=INFINITY
        call maxLevel(eval, switchPhase, sortLimit-1, alpha, beta) ! �l�K�}�b�N�X+�A���t�@�E�x�[�^�@��g�ݍ��킹��

        eval=-eval ! �����̔��]�i�l�K�}�b�N�X�@�j

        ! ���߂��iAI�p�j�Amain�̈�ʗp�ł͑O��̎����̔Ԃɖ߂��Ă������A�����͈��߂�����ł��邱�Ƃɒ���
        call AIundo

        sortEval(n)=eval ! �]���l��ۑ����Ă���
     end do

     do n=1, nm ! �]���l�̍������ɕ��ёւ��i�\�[�g�j

        nSort=maxloc(sortEval(1:nm), mask(1:nm)) ! mask���u�^�v�̈ʒu������Ώۂɍő�l���擾

        xpTmp(n)=xp(nSort(1)) ! x���W�̈ꎞ�ۑ�
        ypTmp(n)=yp(nSort(1)) ! y���W�̈ꎞ�ۑ�

        mask(nSort(1))=.false. ! mask���u�U�v�ɂ��āA���I�񂾍��W�͎��̍ő�l�������珜�O����

     end do
     xp(1:nm)=xpTmp(1:nm) ! �ꎞ�ۑ����琳����x���W�z��Ɏ󂯓n��
     yp(1:nm)=ypTmp(1:nm) ! �ꎞ�ۑ����琳����y���W�z��Ɏ󂯓n��

     deallocate(mask, sortEval, xpTmp, ypTmp) ! �������̉��
  end if


  ! ���ׂĂ̎��ł��Ă���(AI��1��ځ����̎���ŏI�I�Ɍ��肷��)
  evalMax=-INFINITY ! �]���l�̍ő�l��������
  do n=1, nm

     ! �΂�łiAI�p�Amain�̈�ʗp����]���ȑ������菜���A�C���f�b�N�X�𗘗p���΂𗠕Ԃ��Ă���j
     call AImove(xp(n), yp(n))

     ! ���ƃ���������
     alpha=-INFINITY
     beta=INFINITY
     call maxLevel(eval, switchPhase, limit-1, alpha, beta) ! �l�K�}�b�N�X+�A���t�@�E�x�[�^�@��g�ݍ��킹��

     eval=-eval ! �����̔��](�l�K�}�b�N�X�@)

     ! ���߂��iAI�p�j�Amain�̈�ʗp�ł͑O��̎����̔Ԃɖ߂��Ă������A�����͈��߂�����ł��邱�Ƃɒ���
     call AIundo

     if(eval > evalMax) then ! �ŏI�I�Ɉ�ԕ]���l�������肪�I�������
        evalMax=eval
        x=xp(n)
        y=yp(n)
     end if

  end do

  call writeIn(x, y, in) ! �ł�̍��W�𐔒l���當���ɕϊ�

  return
end subroutine treeSearch

!----------------------------------------------------------------

recursive subroutine maxLevel(evalMax, switchPhase, limit, alpha, beta) ! �l�K�}�b�N�X�@+�A���t�@�E�x�[�^�@

  use setParameters, only : &
       ON, OFF, &
       NON, &
       EMPTY, &
       INFINITY, &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       WDL_MODE, &
       PERFECT_MODE, &
       MAX_TURNS
  use setVariables, only : &
       Turns, &
       MovableDir, &
       MemoryRawBoard

  implicit none
  
  integer &
       gameContinue, &
       switchPhase
  integer &
       x, y
  integer, dimension(MAX_TURNS-1):: &
       xp, yp
  integer &
       n, nm, &
       limit
  real &
       eval, evalMax, &
       alpha, beta
  
  evalMax=-INFINITY ! �]���l�̍ő�l��������

  gameContinue=ON ! �Q�[�������s�\���I�����𔻕ʂ̂��߂ɓ���

  call AIisGameOver(gameContinue) ! �Q�[���I�����𔻒�
  if(gameContinue == OFF .or. limit == 0) then ! �Q�[�����I���A�������͐[������ɒB������]������

     ! AI�t�F�[�Y�ɂ���Ďg�p����]���֐��̐؂�ւ�
     if(switchPhase == MIDDLE_MODE) then

        call eval_middleSearch(evalMax) ! ���Ղ̕]���֐�

     else if(switchPhase == WDL_MODE) then

        call eval_WDLSearch(evalMax) ! �I�ՕK���ǂ݂̕]���֐�

     else if(switchPhase == PERFECT_MODE) then

        call eval_perfectSearch(evalMax) ! �I�Պ��S�ǂ݂̕]���֐�

     else
        write(06, '(a)') 'Bad switchPhase.'
        stop
     end if

     return
  end if
  
  ! ���ׂĂ̒u����ʒu�𐶐�
  n=0
  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= NON) then
           n=n+1
           xp(n)=x
           yp(n)=y
        end if
     end do
  end do
  nm=n

  ! �p�X
  if(nm == 0) then

     call AIpass ! �p�X�iAI�p�Amain�̈�ʗp����]���ȑ�������������j
     
     call maxLevel(evalMax, switchPhase, limit, -beta, -alpha) ! �ċA�Ŏ��g�̃T�u���[�`���ɖ߂�

     evalMax=-evalMax ! �����̔��]�i�l�K�}�b�N�X�@�j

     MemoryRawBoard(Turns, :, :)=EMPTY ! ���̎�̋L���Ֆʂ��������i����Ȃ�����...�j

     return
  end if

  ! ���ׂĂ̎��ł��Ă���
  do n=1, nm

     ! �΂�łiAI�p�Amain�̈�ʗp����]���ȑ������菜���A�C���f�b�N�X�𗘗p���΂𗠕Ԃ��Ă���j
     call AImove(xp(n), yp(n))

     call maxLevel(eval, switchPhase, limit-1, -beta, -alpha) ! �ċA�Ŏ��g�̃T�u���[�`���ɖ߂�

     ! ���߂��iAI�p�j�Amain�̈�ʗp�ł͑O��̎����̔Ԃɖ߂��Ă������A�����͈��߂�����ł��邱�Ƃɒ���
     call AIundo

     eval=-eval! �����̔��]�i�l�K�}�b�N�X�@�j

     if(eval >= beta) then ! ���l����������T���I���i�A���t�@�E�x�[�^�@�j
        evalMax=eval
        return
     end if

     if(eval > evalMax) then ! ���ǂ��肪��������
        evalMax=eval
        alpha=max(alpha, evalMax) ! ���l�̍X�V�i�A���t�@�E�x�[�^�@�j
     end if

  end do
  
  return
end subroutine maxLevel

!----------------------------------------------------------------

subroutine AImove(x, y) ! 1��łiAI�p�j

  use setParameters, only : &
       NON, &
       ON, OFF, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MemoryCurrentColor, &
       MemoryRawBoard, &
       CurrentColor, &
       RawBoard

  implicit none

  integer &
       x, y

  MemoryCurrentColor(Turns)=CurrentColor ! ���݂̎�Ԃ�ۑ�
  ! ���݂̃{�[�h����ۑ�
  MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)

  call AIflipDiscs(x, y) ! ���ۂɐ΂𗠕Ԃ��iAI�p�j

  Turns=Turns+1 ! �΂�u�����Ԃ�����A�^�[����i�߂�

  CurrentColor=-CurrentColor ! ��Ԍ��

  call initMovable(CurrentColor) ! MovableDir�i�u����}�X�̏��j���X�V

  return
end subroutine AImove

!----------------------------------------------------------------

subroutine AIundo ! �������iAI�p�j
  use setParameters, only : &
       EMPTY, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       CurrentColor, &
       MemoryCurrentColor, &
       RawBoard, &
       MemoryRawBoard

  implicit none

  Turns=Turns-1 ! �^�[����߂�
  
  CurrentColor=MemoryCurrentColor(Turns) ! 1��O�̃^�[���̐F�ɖ߂�
  ! 1��O�̔Ֆʂɖ߂�
  RawBoard(BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=MemoryRawBoard(Turns, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)
  
  MemoryCurrentColor(Turns+1)=EMPTY ! ��Ԃ̐F��������
  MemoryRawBoard(Turns+1, BOARD_BGN:BOARD_END, BOARD_BGN:BOARD_END)=EMPTY ! �Ֆʂ�������
  
  return
end subroutine AIundo

!----------------------------------------------------------------

subroutine AIisGameOver(gameContinue) ! �Q�[���I�������s���𔻒f�iAI�p�j

  use setParameters, only : &
       ON, OFF, &
       MAX_TURNS, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       Turns, &
       MovableDir, &
       CurrentColor

  implicit none

  integer &
       gameContinue
  integer &
       x, y

  if(Turns == MAX_TURNS) then ! �Ō�̃^�[���̓Q�[���I��
     gameContinue=OFF
     return
  end if

  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= 0) then ! ���΂̒u����ꏊ������΁A�Q�[�����s
           gameContinue=ON
           return
        end if
     end do
  end do

  call initMovable(-CurrentColor) ! ����̒u����ꏊ�̏��ɂ���

  do y=BOARD_BGN, BOARD_END
     do x=BOARD_BGN, BOARD_END
        if(MovableDir(Turns, x, y) /= 0) then ! ���΂��u���Ȃ��Ƃ�����̐΂��u����΃Q�[�����s
           gameContinue=ON
           call initMovable(CurrentColor) ! �u����}�X���������ɖ߂��Ă���
           return
        end if
     end do
  end do

  gameContinue=OFF ! �����܂ł���΂ǂ����������ꏊ���Ȃ��Ƃ������ƁA�Q�[���I��

  return
end subroutine AIisGameOver

!----------------------------------------------------------------

subroutine AIpass ! �p�X����iAI�p�j

  use setParameters, only : &
       ON, OFF, &
       BOARD_BGN, BOARD_END, &
       NON
  use setVariables, only : &
       CurrentColor

  implicit none

  call initMovable(CurrentColor) ! MovableDir�i�u����}�X�̏��j���X�V

  CurrentColor=-CurrentColor ! �p�X�ɂ���Ď�Ԍ��

  call initMovable(CurrentColor) ! MovableDir�i�u����}�X�̏��j���X�V

  return
end subroutine AIpass

!----------------------------------------------------------------

subroutine AIflipDiscs(x, y) ! �΂�u�����Ԃ��iAI�p�j

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BOARD_SIZE, &
       BLACK
  use setVariables, only : &
       RawBoard, &
       CurrentColor
  use setIndexVarias, only : &
       FlipLeft, FlipRight ! �΂�u�����Ƃ����E�i�㉺�A���΂߁j�ɉ��Ԃ��邩������
       
  
  implicit none

  integer &
       x, y, xv, &
       xBlack, xWhite, & ! �u�����}�X�͍��i���j���C����̉��ԖڂɈʒu���邩���i�[
       nColor, & ! BLACK or WHITE�i���������ŕԂ���ΐ����Ⴄ���߁j
       localHorizLine, localVertiLine, & ! �����i�����j���C���̃C���f�b�N�X
       localBlackLine, localWhiteLine, & ! �΂߁i���������͔��j���C���̃C���f�b�N�X
       blackLineNumber, whiteLineNumber ! ���΂߂̃p�^�[����P?�ɂ����邩���i�[����ϐ�

  call getLocalIndex(x, y, & ! �΂�u�����}�X�̏c���΂߃��C���̃C���f�b�N�X���擾����
       localHorizLine, localVertiLine, localBlackLine, localWhiteLine, &
       blackLineNumber, whiteLineNumber)

  if(CurrentColor == BLACK) then ! ����������nColor=1�A����������nColor=2
     nColor=1
  else
     nColor=2
  end if

  ! ���̃}�X�ɐ΂�u��
  RawBoard(x, y)=CurrentColor

  ! �������C���ɂ��āA���E�̐΂𗠕Ԃ�
  RawBoard(x-FlipLeft(localHorizLine, nColor, x-1):x+FlipRight(localHorizLine, nColor, x-1), y)=CurrentColor

  ! �������C���ɂ��āA�㉺�̐΂𗠕Ԃ�
  RawBoard(x, y-FlipLeft(localVertiLine, nColor, y-1):y+FlipRight(localVertiLine, nColor, y-1))=CurrentColor

  ! �����C���ɂ��āA�΂߂̐΂𗠕Ԃ�
  if(localBlackLine /= 3281) then ! �C���f�b�N�X=3281-1=3280��EEEEEEEE�ł���A�Ԃ���΂͂Ȃ��i�R�[�i�[2�}�X�̂��߁j
     if(blackLineNumber <= 6) then
        xBlack=y-1 ! �Ώۃ��C����P6�ȉ��������ꍇ�́A�u�����}�X�̃��C����ł̈ʒu��y-1�ƂȂ�
     else
        xBlack=10-x ! �Ώۃ��C����P7�ȏゾ�����ꍇ�́A�u�����}�X�̃��C����ł̈ʒu��10-x�ƂȂ�
     end if
     if(FlipLeft(localBlackLine, nColor, xBlack) /= 0) then ! �E������̐΂𗠕Ԃ�
        do xv=1, FlipLeft(localBlackLine, nColor, xBlack)
           RawBoard(x+xv, y-xv)=CurrentColor
        end do
     end if
     if(FlipRight(localBlackLine, nColor, xBlack) /= 0) then ! ���������̐΂𗠕Ԃ�
        do xv=1, FlipRight(localBlackLine, nColor, xBlack)
           RawBoard(x-xv, y+xv)=CurrentColor
        end do
     end if
  end if

  ! �����C���ɂ��āA�΂߂̐΂𗠕Ԃ�
  if(localWhiteLine /= 3281) then ! �C���f�b�N�X=3281-1=3280��EEEEEEEE�ł���A�Ԃ���΂͂Ȃ��i�R�[�i�[2�}�X�̂��߁j
     if(whiteLineNumber <= 6) then
        xWhite=10-x ! �Ώۃ��C����P6�ȉ��������ꍇ�́A�u�����}�X�̃��C����ł̈ʒu��10-x�ƂȂ�
     else
        xWhite=10-y  ! �Ώۃ��C����P7�ȏゾ�����ꍇ�́A�u�����}�X�̃��C����ł̈ʒu��10-y�ƂȂ�
     end if
     if(FlipLeft(localWhiteLine, nColor, xWhite) /= 0) then ! �E�������̐΂𗠕Ԃ�
        do xv=1, FlipLeft(localWhiteLine, nColor, xWhite)
           RawBoard(x+xv, y+xv)=CurrentColor
        end do
     end if
     if(FlipRight(localWhiteLine, nColor, xWhite) /= 0) then ! ��������̐΂𗠕Ԃ�
        do xv=1, FlipRight(localWhiteLine, nColor, xWhite)
           RawBoard(x-xv, y-xv)=CurrentColor
        end do
     end if
  end if

  return
end subroutine AIflipDiscs

!----------------------------------------------------------------

subroutine getLocalIndex(x, y, & ! �΂�u�����}�X�̏c���΂߃��C���ɂ��ẴC���f�b�N�X���擾
     localHorizLine, localVertiLine, localBlackLine, localWhiteLine, &
     blackLineNumber, whiteLineNumber)

  use setParameters, only : &
       BOARD_BGN, BOARD_END, &
       BOARD_SIZE
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       x, y, & ! ���W
       index, & ! �C���f�b�N�X
       blackLineNumber, whiteLineNumber ! ���΂߂̃p�^�[����P?�ɂ����邩���i�[����ϐ�
  integer &
       localHorizLine, & ! �������C���̃C���f�b�N�X
       localVertiLine, & ! �������C���̃C���f�b�N�X
       localBlackLine, & ! �����C���̃C���f�b�N�X
       localWhiteLine    ! �����C���̃C���f�b�N�X

  ! �������C���̃C���f�b�N�X���擾
  index= &
       3*(3*(3*(3*(3*(3*(3*RawBoard(BOARD_END, y) &
       +RawBoard(BOARD_END-1, y)) &
       +RawBoard(BOARD_END-2, y)) &
       +RawBoard(BOARD_END-3, y)) &
       +RawBoard(BOARD_BGN+3, y)) &
       +RawBoard(BOARD_BGN+2, y)) &
       +RawBoard(BOARD_BGN+1, y)) &
       +RawBoard(BOARD_BGN, y)
  localHorizLine=index+3281 ! �z��v�f�ԍ��ɕϊ����Ă���
                            ! �C���f�b�N�X��0000����Ȃ̂ɑ΂��āA�z���1����ł��邱�Ƃɗ���

  ! �������C���̃C���f�b�N�X���擾
  index= &
       3*(3*(3*(3*(3*(3*(3*RawBoard(x, BOARD_END) &
       +RawBoard(x, BOARD_END-1)) &
       +RawBoard(x, BOARD_END-2)) &
       +RawBoard(x, BOARD_END-3)) &
       +RawBoard(x, BOARD_BGN+3)) &
       +RawBoard(x, BOARD_BGN+2)) &
       +RawBoard(x, BOARD_BGN+1)) &
       +RawBoard(x, BOARD_BGN)
  localVertiLine=index+3281 ! �z��v�f�ԍ��ɕϊ����Ă���

  ! �����C���̃C���f�b�N�X���擾
  blackLineNumber=x+y-5 ! ���C���ԍ�(P?)���擾
  select case (blackLineNumber) ! ���C���ԍ��ɉ����ă}�X�����ς�邽�ߏꍇ����
  case(1) ! P1���C��
     index= &
          3*(3*RawBoard(BOARD_BGN, BOARD_BGN+2) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN)
     localBlackLine=index+3281
  case(2) ! P2���C��
     index= &
          3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+3) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN)
     localBlackLine=index+3281
  case(3) ! P3���C��
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+4) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN)
     localBlackLine=index+3281
  case(4) ! P4���C��
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+5) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN)
     localBlackLine=index+3281
  case(5) ! P5���C��
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+6) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN)
     localBlackLine=index+3281
  case(6) ! P6���C��
     index= &
          3*(3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_END) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+1)) &
          +RawBoard(BOARD_END, BOARD_BGN)
     localBlackLine=index+3281
  case(7) ! P7���C��
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN+1, BOARD_END) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+2)) &
          +RawBoard(BOARD_END, BOARD_BGN+1)
     localBlackLine=index+3281
  case(8) ! P8���C��
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN+2, BOARD_END) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+3)) &
          +RawBoard(BOARD_END, BOARD_BGN+2)
     localBlackLine=index+3281
  case(9) ! P9���C��
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN+3, BOARD_END) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+4)) &
          +RawBoard(BOARD_END, BOARD_BGN+3)
     localBlackLine=index+3281
  case(10) ! P10���C��
     index= &
          3*(3*(3*RawBoard(BOARD_BGN+4, BOARD_END) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+5)) &
          +RawBoard(BOARD_END, BOARD_BGN+4)
     localBlackLine=index+3281
  case(11) ! P11���C��
     index= &
          3*(3*RawBoard(BOARD_BGN+5, BOARD_END) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+6)) &
          +RawBoard(BOARD_END, BOARD_BGN+5)
     localBlackLine=index+3281
  case default ! �����̓R�[�i�[2�}�X�̂Ƃ������Ă���
     localBlackLine=3281 ! EEEEEEEE�ł��邱�Ƃɂ��Ă���
  end select

  ! �����C���̃C���f�b�N�X���擾
  whiteLineNumber=6-x+y ! ���C���ԍ�(P?)���擾
  select case (whiteLineNumber) ! ���C���ԍ��ɉ����ă}�X�����ς�邽�ߏꍇ����
  case(1) ! P1���C��
     index= &
          3*(3*RawBoard(BOARD_BGN+5, BOARD_BGN) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+1)) &
          +RawBoard(BOARD_END, BOARD_BGN+2)
     localWhiteLine=index+3281
  case(2) ! P2���C��
     index= &
          3*(3*(3*RawBoard(BOARD_BGN+4, BOARD_BGN) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+2)) &
          +RawBoard(BOARD_END, BOARD_BGN+3)
     localWhiteLine=index+3281
  case(3) ! P3���C��
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN+3, BOARD_BGN) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+3)) &
          +RawBoard(BOARD_END, BOARD_BGN+4)
     localWhiteLine=index+3281
  case(4) ! P4���C��
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN+2, BOARD_BGN) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+4)) &
          +RawBoard(BOARD_END, BOARD_BGN+5)
     localWhiteLine=index+3281
  case(5) ! P5���C��
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN+1, BOARD_BGN) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+5)) &
          +RawBoard(BOARD_END, BOARD_BGN+6)
     localWhiteLine=index+3281
  case(6) ! P6���C��
     index= &
          3*(3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+1)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+6, BOARD_BGN+6)) &
          +RawBoard(BOARD_END, BOARD_END)
     localWhiteLine=index+3281
  case(7) ! P7���C��
     index= &
          3*(3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+1) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+2)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+5, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+6, BOARD_END)
     localWhiteLine=index+3281
  case(8) ! P8���C��
     index= &
          3*(3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+2) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+3)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+4, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+5, BOARD_END)
     localWhiteLine=index+3281
  case(9) ! P9���C��
     index= &
          3*(3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+3) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+4)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+3, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+4, BOARD_END)
     localWhiteLine=index+3281
  case(10) ! P10���C��
     index= &
          3*(3*(3*RawBoard(BOARD_BGN, BOARD_BGN+4) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+5)) &
          +RawBoard(BOARD_BGN+2, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+3, BOARD_END)
     localWhiteLine=index+3281
  case(11) ! P11���C��
     index= &
          3*(3*RawBoard(BOARD_BGN, BOARD_BGN+5) &
          +RawBoard(BOARD_BGN+1, BOARD_BGN+6)) &
          +RawBoard(BOARD_BGN+2, BOARD_END)
     localWhiteLine=index+3281
  case default ! �����̓R�[�i�[2�}�X�̂Ƃ������Ă���
     localWhiteLine=3281 ! EEEEEEEE�ł��邱�Ƃɂ��Ă���
  end select

  return
end subroutine getLocalIndex

!----------------------------------------------------------------

subroutine theBook(in, switchPhase, evalMax) ! ��΂�T��

  use setParameters, only : &
       ON, OFF, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       BOARD_SIZE, &
       INFINITY
  use userSet, only : &
       AI_COLOR
  use setTheBookParams, only : &
       THEBOOKWAY ! �g�p�����΃p�^�[���̐�
  use setVariables, only : &
       MemoryPutPlace, &
       Turns
  use setTheBookVarias, only : &
       WayBook, & ! ��΂̊������i�[�����
       EvalBook   ! ��΂̕]���l

  implicit none

  integer &
       n, m
  integer &
       x, y, &    ! �ϊ��O�̍��W
       xNew, yNew ! �ϊ���̍��W
  integer &
       consistBook, & ! ��΃t�@�C���ɏ����ꂽ�����Ƃ̈�v�𔻒f����ϐ�
       switchPhase
  real &
       eval, evalMax
  character(len=2) &
       in
  integer, allocatable:: &
       rotatedPutPlace(:)

  ! ������
  x=0
  y=0

  ! Turns=1�̏ꍇ��f5��Ԃ�
  if(Turns == 1) then
     in='f5'
     return
  end if


  ! �ߋ��̗���(MemoryPutPlace)�̉�]����(1��ڂ�f5�ƂȂ�悤�ɂ���)
  allocate(rotatedPutPlace(1:Turns-1))

  select case (MemoryPutPlace(1))
  case(54) ! d3�A���v�����90�x��] + x���ɑ΂��Ĕ��]

     do m=1, Turns-1
        x=int(MemoryPutPlace(m)*0.1) ! ��]�O��x���W
        y=mod(MemoryPutPlace(m), 10) ! ��]�O��y���W
        xNew=(BOARD_SIZE+2)-y+1      ! ��]���x���W
        yNew=(BOARD_SIZE+2)-x+1      ! ��]���y���W
        rotatedPutPlace(m)=xNew*10+yNew
     end do

  case(45) ! c4�A180�x��]

     do m=1, Turns-1
        x=int(MemoryPutPlace(m)*0.1) ! ��]�O��x���W
        y=mod(MemoryPutPlace(m), 10) ! ��]�O��y���W
        xNew=(BOARD_SIZE+2)-x+1      ! ��]���x���W
        yNew=(BOARD_SIZE+2)-y+1      ! ��]���y���W
        rotatedPutPlace(m)=xNew*10+yNew
     end do

  case(67) ! e6�A�����v�����90�x��] + x���ɑ΂��Ĕ��]

     do m=1, Turns-1
        x=int(MemoryPutPlace(m)*0.1) ! ��]�O��x���W
        y=mod(MemoryPutPlace(m), 10) ! ��]�O��y���W
        xNew=y                       ! ��]���x���W
        yNew=x                       ! ��]���y���W
        rotatedPutPlace(m)=xNew*10+yNew
     end do
     
  case default ! ��]������K�v����

     rotatedPutPlace(1:Turns-1)=MemoryPutPlace(1:Turns-1) ! ���̂܂ܑ��

  end select


  ! ��΂�T��
  switchPhase=MIDDLE_MODE ! �Q�[���t�F�[�Y�̏������A�����ɓ����Ă���Ƃ���OPENING_MODE
  evalMax=-INFINITY ! �]���l�̏�����
  do n=1, THEBOOKWAY

     consistBook=ON ! �ߋ��̗�������Βʂ�
     do m=1, Turns-1
        if(rotatedPutPlace(m) /= WayBook(n, m)) then
           consistBook=OFF ! �ߋ��̗�������΂ɏ]���Ă��Ȃ�
           exit
        end if
     end do
     if(WayBook(n, Turns) == 0) consistBook=OFF ! �ߋ��̗����͒�Βʂ�ł��邪�A���łĂ��΂�����

     eval=EvalBook(n)*AI_COLOR ! AI���_�̕]���l�ɕϊ�
     if(consistBook == ON .and. eval >= 0) then ! ��΂�����A���̕]���l�������ɗL���ł���

        if(eval > evalMax) then ! �]���l���ő�̂��̂�I��
           x=int(WayBook(n, Turns)*0.1)
           y=mod(WayBook(n, Turns), 10)
           evalMax=eval
           switchPhase=OPENING_MODE ! ��΂���Ȃ̂ŁA���̃^�[������΂�T���ɗ���
        end if

     end if

  end do
  if(switchPhase == MIDDLE_MODE) return ! ��΂Ȃ��Ȃ̂ŁA���̃^�[������͒�ΒT���͍s��Ȃ�


  ! ���̍��W�ɖ߂���]����(�ł�̂�)
  select case (MemoryPutPlace(1))
  case(54) ! d3�A���v�����90�x��] + x���ɑ΂��Ĕ��]
     xNew=(BOARD_SIZE+2)-y+1
     yNew=(BOARD_SIZE+2)-x+1
  case(45) ! c4�A180�x��]
     xNew=(BOARD_SIZE+2)-x+1
     yNew=(BOARD_SIZE+2)-y+1
  case(67) ! e6�A�����v�����90�x��] + x���ɑ΂��Ĕ��]
     xNew=y
     yNew=x
  case default ! �ϊ��̕K�v����
     xNew=x
     yNew=y
  end select

  deallocate(rotatedPutPlace) ! �z��̊J��

  call writeIn(xNew, yNew, in) ! �ł�̐��l�\���𕶎��\���ɕϊ�

  return
end subroutine theBook

!----------------------------------------------------------------

subroutine getHorizontal1Index(horizPLine) ! �����p�^�[��1�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       horizPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 2, 2, 2, 2, 2, 2, 2, 2 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|@ @ @ @ @ @ @ @|
  ! 2|               |
  ! 3|               |
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! �Ώ̎��̃^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal1Index

!----------------------------------------------------------------

subroutine getHorizontal2Index(horizPLine) ! �����p�^�[��2�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       horizPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 3, 3, 3, 3, 3, 3, 3, 3 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|               |
  ! 2|@ @ @ @ @ @ @ @|
  ! 3|               |
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! �Ώ̎��̃^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal2Index

!----------------------------------------------------------------

subroutine getHorizontal3Index(horizPLine) ! �����p�^�[��3�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       horizPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 4, 4, 4, 4, 4, 4, 4, 4 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|               |
  ! 2|               |
  ! 3|@ @ @ @ @ @ @ @|
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! �Ώ̎��̃^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal3Index

!----------------------------------------------------------------

subroutine getHorizontal4Index(horizPLine) ! �����p�^�[��4�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       horizPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 5, 5, 5, 5, 5, 5, 5, 5 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|               |
  ! 2|               |
  ! 3|               |
  ! 4|@ @ @ @ @ @ @ @|
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=BOARD_SIZE/2 ! �Ώ̎��̃^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     horizPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getHorizontal4Index

!----------------------------------------------------------------

subroutine getDiagonal1Index(diagoPLine) ! �΂߃p�^�[��1�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! �p�^�[�����\������}�X�̐�
       lm=2 ! ��]���쐔
  integer, dimension(lm):: &
       diagoPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 2, 3, 4, 5, 6, 7, 8, 9 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|@              |
  ! 2|  @            |
  ! 3|    @          |
  ! 4|      @ X      |
  ! 5|      X @      |
  ! 6|          @    |
  ! 7|            @  |
  ! 8|              @|
  !   ---------------
  ! type: x=y ! �Ώ̎��^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal1Index

!----------------------------------------------------------------

subroutine getDiagonal2Index(diagoPLine) ! �΂߃p�^�[��2�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=7, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       diagoPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 3, 4, 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 2, 3, 4, 5, 6, 7, 8 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|  @            |
  ! 2|    @          |
  ! 3|      @        |
  ! 4|      O @      |
  ! 5|      X O @    |
  ! 6|            @  |
  ! 7|              @|
  ! 8|               |
  !   ---------------
  ! type: x=y ! �Ώ̎��^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal2Index

!----------------------------------------------------------------

subroutine getDiagonal3Index(diagoPLine) ! �΂߃p�^�[��3�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=6, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       diagoPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 4, 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 2, 3, 4, 5, 6, 7 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|    @          |
  ! 2|      @        |
  ! 3|        @      |
  ! 4|      O X @    |
  ! 5|      X O   @  |
  ! 6|              @|
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal3Index

!----------------------------------------------------------------

subroutine getDiagonal4Index(diagoPLine) ! �΂߃p�^�[��4�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=5, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       diagoPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 5, 6, 7, 8, 9 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 2, 3, 4, 5, 6 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|      @        |
  ! 2|        @      |
  ! 3|          @    |
  ! 4|      O X   @  |
  ! 5|      X O     @|
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y ! �Ώ̎��^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal4Index

!----------------------------------------------------------------

subroutine getDiagonal5Index(diagoPLine) ! �΂߃p�^�[��5�̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=4, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       diagoPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: & ! �p�^�[�����\������}�X�̔z��
       x= (/ 6, 7, 8, 9 /), &
       y= (/ 2, 3, 4, 5 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|        @      |
  ! 2|          @    |
  ! 3|            @  |
  ! 4|      O X     @|
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y ! �Ώ̎��^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     diagoPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getDiagonal5Index

!----------------------------------------------------------------

subroutine getCornerIndex(cornerPLine) ! �R�[�i�[�p�^�[���̃C���f�b�N�X���擾����

  use setParameters, only : &
       BOARD_SIZE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       RawBoard

  implicit none

  integer &
       n, l, &
       xNew, yNew, &
       index
  integer, parameter:: &
       nm=8, & ! �p�^�[�����\������}�X�̐�
       lm=4 ! ��]���쐔
  integer, dimension(lm):: &
       cornerPLine ! �C���f�b�N�X���i�[
  integer, dimension(nm):: &
       x= (/ 2, 3, 4, 2, 3, 4, 2, 3 /), & ! �p�^�[�����\������}�X�̔z��
       y= (/ 2, 2, 2, 3, 3, 3, 4, 4 /)

  !   a b c d e f g h 
  !   ---------------
  ! 1|@ @ @          |
  ! 2|@ @ @          |
  ! 3|@ @            |
  ! 4|      O X      |
  ! 5|      X O      |
  ! 6|               |
  ! 7|               |
  ! 8|               |
  !   ---------------
  ! type: x=y ! �Ώ̎��^�C�v

  do l=1, lm ! ��]���[�v
     index=0

     do n=1, nm ! �}�X���[�v
        index=index+RawBoard(x(n), y(n))*3**(n-1)
     end do
     cornerPLine(l)=index+3281

     do n=1, nm ! ���v�����90�x��]
        xNew=(BOARD_SIZE+2)-y(n)+1
        yNew=x(n)
        x(n)=xNew
        y(n)=yNew
     end do

  end do

  return
end subroutine getCornerIndex
