program main ! ���C���v���O����

  use setParameters, only : &
       ON, OFF, DIS, &
       MAX_TURNS, &
       BLACK
  use userSet, only : &
       GAMES_NUMBER, &
       RANDOM_PROCESS, &
       INITIAL_PHASE
  use setVariables, only : &
       init, & ! �T�u���[�`��
       Turns, &
       CurrentColor, &
       DiscsBlack, DiscsWhite, &
       Player1, Player2
!V  use setTheBookVarias, only : &
!V       initTheBook ! �T�u���[�`��
  use setIndexVarias, only : &
       initIndex ! �T�u���[�`��
  use setPatternEvalVarias, only : &
       initPatternEval_1, & ! �T�u���[�`��
       initPatternEval_2    ! �T�u���[�`��

  implicit none

  integer &
       correctIn, &
       gameRetire, &
       gameContinue, &
       switchPhase1, &
       switchPhase2, &
       x, y
  integer, dimension(8):: &
       values
  character(len=2) &
       in
  character(len=8) &
       date
  character(len=10) &
       time
  character(len=5) &
       zone
  integer &
       player1Win, &
       draw, &
       player2Win, &
       numOfGames
  character(len=6) &
       str_numOfGames

  ! �t�@�C�����̂��߂Ɏ��Ԃ��擾
  call date_and_time(date, time, zone, values)
  open(01, file='out/'//date//time)
  write(06, '(a)') 'file name = '//date//time

  ! �����ݒ���o��
  call initialSetting

  ! ������
  write(06, '(a)') 'initializing...'
  call initIndex ! �C���f�b�N�X�\��z��ɑ��
  call initPatternEval_1 ! �p�^�[���]���l�\��z��ɑ��(player1)
  call initPatternEval_2 ! �p�^�[���]���l�\��z��ɑ��(player2)
  player1Win=0
  draw=0
  player2Win=0

  do numOfGames=1, GAMES_NUMBER ! ���������[�v
     
     call init
     gameContinue=ON
     gameRetire  =OFF
     switchPhase1=INITIAL_PHASE
     switchPhase2=INITIAL_PHASE
     Player1=(-1)**numOfGames ! ��U��U��1�Q�[�����ɓ���ւ���
     Player2=-Player1

     write(str_numOfGames, '(i6)') numOfGames
     call output('Game Number :'//str_numOfGames, ON)

     ! �΋ǂ̊J�n
     do ! �΋ǃ��[�v�F�΋ǂ��I���΁A���[�v�O�ɏo��
        
        correctIn=OFF
        
        call pass(correctIn)
        
        do ! ��ԃ��[�v�F�������肪�ł����΁A���[�v�O�ɏo��
           
           if(Turns <= RANDOM_PROCESS) then ! �����_���i�s�萔
              
              call random(in)

           else

              if(Player1 == CurrentColor) then ! player1�̔�
              
                 call ai(switchPhase1, in, Player1)
                 
              else ! player2�̔�
              
                 call ai(switchPhase2, in, Player2)
              
              end if
              
           end if

           call readIn(x, y, correctIn, in)
           
           call move(x, y, correctIn)
           
           if(correctIn == OFF) then
              call output('incorrect input.', ON)
           else
              exit
           end if
              
        end do
        
        call isGameOver(gameRetire, gameContinue)
        
        if(gameContinue == OFF) exit
     end do

     ! �����̌v�Z
     if(Player1 == BLACK) then
        if(DiscsBlack > DiscsWhite) then
           player1Win=player1Win+1
        else if(DiscsBlack < DiscsWhite) then
           player2Win=player2Win+1
        else
           draw=draw+1
        end if
     else
        if(DiscsWhite > DiscsBlack) then
           player1Win=player1Win+1
        else if(DiscsWhite < DiscsBlack) then
           player2Win=player2Win+1
        else
           draw=draw+1
        end if
     end if
     
     call writePutPlace

  end do

  call rateOfWin(player1Win, draw, player2Win) ! �����̏o��
  
  close(01)

end program main
