program main ! ���C���v���O����

  use setParameters, only : &
       ON, OFF, DIS, &
       MAX_TURNS, &
       OPENING_MODE, &
       MIDDLE_MODE, &
       WDL_MODE, &
       PERFECT_MODE
  use userSet, only : &
       AUTO_IN, &
       AI_COLOR, &
       NORMAL_DEPTH, &
       WDL_DEPTH, &
       PERFECT_DEPTH, &
       SORT_DEPTH, &
       USE_RANDOM
  use setVariables, only : &
       init, & ! �T�u���[�`��
       Turns, &
       CurrentColor
  use setTheBookVarias, only : &
       initTheBook ! �T�u���[�`��
  use setIndexVarias, only : &
       initIndex ! �T�u���[�`��
  use setPatternEvalVarias, only : &
       initPatternEval ! �T�u���[�`��

  implicit none

  integer &
       correctIn, &    ! �ł����肪���������i�łĂ邩�j�𔻕� (ON or OFF)
       gameRetire, &   ! �Q�[�����~���ǂ����𔻕� (ON or OFF)
       gameContinue, & ! �Q�[���I�����ǂ����𔻕� (ON or OFF)
       switchPhase, &  ! AI�p�ɃQ�[���t�F�[�Y�𔻕�
                       ! (OPENING_MODE or MIDDLE_MODE or WDL_MODE or PERFECT_MODE)
       x, y            ! �Ֆʂ̍��W��
  integer*8 &
       time0, time1, dtime ! AI�v�l���Ԃ̎擾�p
  real*8 &
       computingTime, &   ! AI�v�l���ԁi1��j
       totalComputingTime ! AI�v�l���ԁi�Q�[���g�[�^���j
  character(len=8) &
       str_eval ! �]���l�̃A�E�g�v�b�g�p�̕�����
  character(len=2) &
       in ! �ł�̓��͗p�i��Ff5�j
  character(len=7) &
       phase ! �Q�[���t�F�[�Y�̃A�E�g�v�b�g�p�̕�����
  character(len=120) &
       fileIn ! �����Ֆʐݒ�p�̊����t�@�C������ǂݍ��ޕ�����
  character(len=8) &
       date ! �t�@�C�����̂��߂̔N����
  character(len=10) &
       time ! �t�@�C�����̂��߂̎����b.�R���}�b
  character(len=6) &
       str_computingTime ! AI�v�l���Ԃ̃A�E�g�v�b�g�p�̕�����i1��j
  character(len=8) &
       str_totalcomputingTime ! AI�v�l���Ԃ̃A�E�g�v�b�g�p�̕�����i�Q�[���g�[�^���j

  ! �t�@�C�����̂��߂Ɏ��Ԃ��擾
  call date_and_time(date, time)
  open(01, file='out/boardHistory.dat') ! �Q�[���L�^�i�r�W���A���I�j
  open(02, file='out/'//date//time)     ! �Q�[���L�^�i�V���v�������j
  call output('file name = '//date//time, ON) ! �t�@�C�����̕\��

  ! ������
  call output('initializing...', ON)
  call init            ! �Q�[���S�ʂɊւ��鏉����
  call initTheBook     ! ��΂Ɋւ��鏉����
  call initIndex       ! �C���f�b�N�X�Ɋւ��鏉����
  call initPatternEval ! �p�^�[���]���l�Ɋւ��鏉����
  gameContinue=ON           ! �Q�[�����s��ON
  gameRetire  =OFF          ! �Q�[�����~��OFF
  switchPhase =OPENING_MODE ! �Q�[���t�F�[�Y��OPENING_MODE
  totalComputingTime=0.     ! AI�v�l�Q�[���g�[�^�����Ԃ̏�����
  if(AUTO_IN == ON) then    ! �����Ֆʂ̓��͗p��fileIn�Ɋ������i�[
     open(10, file='dat/fileIn.dat')
     read(10, '(a)') fileIn
     close(10)
  end if

  ! �΋ǂ̊J�n
  call output('------- begin -------', ON)

  do ! �΋ǃ��[�v�F�΋ǂ��I���΁A���[�v�O�ɏo��

     correctIn=OFF ! �ł�����̐������ɂ���

     call pass(correctIn) ! �p�X����A�p�X�̏ꍇ�̓^�[�����`�F���W�i���̔��j

     call output(' ', DIS) ! �Ֆʏo��

     do ! ��ԃ��[�v�F�������肪�ł����΁A���[�v�O�ɏo��

        call output('coord:', OFF) ! �ł�������o��

        if(Turns <= len_trim(fileIn)/2 .and. AUTO_IN == ON) then ! �����Ֆʓ��͂�ON�̏ꍇ

           in=fileIn(2*Turns-1:2*Turns) ! fileIn������Â��o����

        else

           if(AI_COLOR == CurrentColor) then ! AI�̔�

              call system_clock(time0) ! �v�Z���ԑ���i�J�n�j
              call output('computing...', ON)

              call ai(switchPhase, in, str_eval, phase) ! AI�T�u���[�`���A�����ŃR���s���[�^�̎�iin�j�����肷��
              call output(in, OFF) ! ��iin�j�̏o��
              call output(' ('//trim(phase)//','//str_eval//')', ON) ! �Q�[���t�F�[�Y�ƕ]���l�̏o��
              call system_clock(time1, dtime) ! �v�Z���ԑ���i�I���j
              computingTime=1d0*(time1-time0)/dtime ! �v�Z���ԁi1��j
              totalComputingTime=totalComputingTime+computingTime! �v�Z���ԁi1��j��ώZ���ăg�[�^�����Ԃɂ��Ă���
              write(str_computingTime, '(f6.3)') computingTime 
              call output('compute time ='//str_computingTime//'s', ON) ! �v�Z���Ԃ̏o��

           else ! �l�Ԃ̔�

              if(USE_RANDOM == ON) then ! USE_RANDOM��ON�ɂ��Ă�����A�����̎�������_���֐��ɔC����
                 call random(in)
              else
                 read(05, '(a)') in ! �����̎���^�[�~�i���������
              end if

              call output(in, ON) ! ��iin�j�̏o��

           end if

        end if

        if(in == 'u') then ! 'u'���͂��󂯂āA���߂�
           call unundo(correctIn)
           x=0
           y=0
        else if(in == 'r') then ! 'r'���͂��󂯂āA�Q�[���𒆎~����
           call retireGame(correctIn, gameRetire)
           x=0
           y=0
        else
           call readIn(x, y, correctIn, in) ! in�����W�ɕϊ�����
           call move(x, y, correctIn) ! ���ۂɔՖʂɐ΂�u���āA�΂�Ԃ�
           if(correctIn == OFF) call output('incorrect input.', ON) ! �肪�������Ȃ��i�΂��u���Ȃ��j�Ƃ��Ɍx��
        end if

        if(correctIn == ON) exit ! �������肪�ł����΁A���[�v�𔲂��Ď��̃^�[���Ɉڂ�
     end do

     call output(' ', ON) ! ���s

     call isGameOver(gameRetire, gameContinue) ! �Q�[���I�����������~�𔻒f

     if(gameContinue == OFF) exit ! �Q�[���I���E���~�igameContinue=OFF�j�̂Ƃ����[�v�𔲂��Q�[�����I���
  end do

  call output(' ', DIS) ! �Ō�̔Ֆʂ��o��

  write(str_totalComputingTime, '(f8.3)') totalComputingTime
  call output('total compute time ='//str_totalcomputingTime//'s', ON) ! AI�̎v�l���ԁi�Q�[���g�[�^���j���o��
  call output('-------- end --------', ON)

  call writePutPlace ! file.02�ɃQ�[���L�^�A�V���v������

  close(01)
  close(02)

end program main
