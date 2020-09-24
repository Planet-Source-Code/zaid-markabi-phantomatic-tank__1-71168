VERSION 5.00
Object = "{C1A8AF28-1257-101B-8FB0-0020AF039CA3}#1.1#0"; "MCI32.OCX"
Begin VB.Form Form1 
   BorderStyle     =   0  'None
   Caption         =   "Zaid Markabi"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   -105
   ClientWidth     =   12000
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9000
   ScaleWidth      =   12000
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      BackColor       =   &H00808080&
      ForeColor       =   &H80000008&
      Height          =   4530
      Left            =   0
      ScaleHeight     =   4500
      ScaleWidth      =   11985
      TabIndex        =   1
      Top             =   4440
      Width           =   12015
      Begin VB.PictureBox Picture4 
         Appearance      =   0  'Flat
         BackColor       =   &H00C00000&
         ForeColor       =   &H80000008&
         Height          =   735
         Left            =   0
         ScaleHeight     =   705
         ScaleWidth      =   11985
         TabIndex        =   4
         Top             =   3795
         Width           =   12015
         Begin VB.Label Label2 
            Alignment       =   2  'Center
            BackColor       =   &H00C00000&
            Caption         =   "Health : 100"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   24
               Charset         =   178
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H00FFFFFF&
            Height          =   495
            Left            =   0
            TabIndex        =   5
            Top             =   105
            Width           =   11775
         End
      End
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H00808080&
      ForeColor       =   &H80000008&
      Height          =   4620
      Left            =   0
      ScaleHeight     =   4590
      ScaleWidth      =   11985
      TabIndex        =   0
      Top             =   -120
      Width           =   12015
      Begin MCI.MMControl Snd 
         Height          =   375
         Left            =   120
         TabIndex        =   6
         Top             =   240
         Visible         =   0   'False
         Width           =   420
         _ExtentX        =   741
         _ExtentY        =   661
         _Version        =   393216
         PrevVisible     =   0   'False
         NextVisible     =   0   'False
         PauseVisible    =   0   'False
         BackVisible     =   0   'False
         StepVisible     =   0   'False
         StopVisible     =   0   'False
         RecordVisible   =   0   'False
         EjectVisible    =   0   'False
         DeviceType      =   ""
         FileName        =   ""
      End
      Begin VB.PictureBox Picture3 
         Appearance      =   0  'Flat
         BackColor       =   &H000000C0&
         ForeColor       =   &H80000008&
         Height          =   735
         Left            =   0
         ScaleHeight     =   705
         ScaleWidth      =   11985
         TabIndex        =   2
         Top             =   3820
         Width           =   12015
         Begin VB.Label Label1 
            Alignment       =   2  'Center
            BackColor       =   &H000000C0&
            Caption         =   "Health : 100"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   24
               Charset         =   178
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H00FFFFFF&
            Height          =   495
            Left            =   0
            TabIndex        =   3
            Top             =   105
            Width           =   11775
         End
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private TV3D As TVEngine
'Private Atmos As TVAtmosphere

Private Ppls(50) As TVActor
Private AllPpls As Integer
Private PplsDir(50) As String
Private PplsGT(50) As D3DVECTOR
Private PplsGif(50) As String
Private PplsFrid(50) As Integer
Private PplsDie(50) As Boolean
Private PplsLastFrame(50) As Integer
Private LastPplBuild As String

Private Bombing(10, 2) As TVMesh
Private BombingOF(10) As Boolean
Private BombingPos(10) As D3DVECTOR
Private BombingScal(10) As Single
Private BombingFree As Integer
Private BombingFreeBig(10) As Integer

Private TankDB1(19) As TVMesh
Private TankDB1Direction(19) As D3DVECTOR
Private TankDB1Destination(19) As D3DVECTOR
Private TankDB1DirectionNM(19) As Integer
Private TankDB1DestinationNM(19) As Integer
Private TankDB1DestinationN(19) As Integer
Private TankDBH1(19) As Integer
Private TankDBH1Wait As Integer

Private TankDB2(19) As TVMesh
Private TankDB2Direction(19) As D3DVECTOR
Private TankDB2Destination(19) As D3DVECTOR
Private TankDB2DirectionNM(19) As Integer
Private TankDB2DestinationNM(19) As Integer
Private TankDB2DestinationN(19) As Integer
Private TankDBH2(19) As Integer
Private TankDBH2Wait As Integer

Private TankDBDirection(20, 5) As D3DVECTOR
Private TankDBDestination(20, 5) As D3DVECTOR
Private TankDBDirection2(20, 5) As D3DVECTOR
Private TankDBDestination2(20, 5) As D3DVECTOR
Private AllDBDestination(5) As Integer
Private AllDBDestination2(5) As Integer
Private AllDBDestinationP1 As Integer
Private AllDBDestinationP2 As Integer
Private MKR As D3DVECTOR
Private MKR2 As D3DVECTOR
Private LvlColl(500) As D3DVECTOR
Private LvlMesh(500) As TVMesh
Private AllLvlMesh As Integer
Private AllLvlColl As Integer
Private P1Money As Integer
Private P2Money As Integer
Private P1Gn As Integer
Private P2Gn As Integer

Private P1GnB As Boolean
Private P2GnB As Boolean
Private P1GnW As Integer
Private P2GnW As Integer

Private PWin As Integer
Private PWinWait As Integer

Private TankActor As TVMesh
Private TankActorH As Integer
Private TankActor2 As TVMesh
Private TankActor2H As Integer
Private Gun1ON As Boolean
Private Gun2ON As Boolean
Private Tank1Direction As D3DVECTOR
Private Tank2Direction As D3DVECTOR
Private Tank1Destination As D3DVECTOR
Private Tank2Destination As D3DVECTOR
Private TankActorGun As TVMesh
Private TankActor2Gun As TVMesh
Private Gun1Kill As Integer
Private Gun2Kill As Integer

Private TankGActor(50) As TVMesh
Private TankGActorFC(50) As String
Private TankGActorG(50) As TVMesh
Private AllTankGActor As Integer
Private TankGActorH(50) As Integer
Private TankGActorGP As D3DVECTOR

Private ViewModeP1 As Integer
Private ViewModeP2 As Integer

Private Level As TVMesh

Private TextureFactory As TVTextureFactory

Private Scene As TVScene

Private InputEngine As TVInputEngine

Private DoLoop As Boolean

Private Viewport1 As TVViewport
Private Viewport2 As TVViewport

Private TankPosition As D3DVECTOR
Private Tank2Position As D3DVECTOR

Private TankPositionF As D3DVECTOR
Private Tank2PositionF As D3DVECTOR

Private sngAngleX As Single
Private sngAngleY As Single
Private sngWalk As Single
Private sngStrafe As Single
Private sngBrake As Single

Private sng2AngleX As Single
Private sng2AngleY As Single
Private sng2Walk As Single
Private sng2Strafe As Single
Private sng2Brake As Single

Private I As Integer
Private I2 As Integer
Private I3 As Integer
Private I4 As Integer
Private I5 As Integer
Private X0 As String
Private X1 As String

Private Pl1Pos As D3DVECTOR
Private Pl2Pos As D3DVECTOR

Private Sub Form_Load()
    Set TV3D = New TVEngine

    TV3D.SetSearchDirectory App.Path
    
    TV3D.Init3DWindowedMode Picture1.hWnd

    Set InputEngine = New TVInputEngine

   Set Viewport1 = TV3D.CreateViewport(Picture1.hWnd, "Viewport1")
   Set Viewport2 = TV3D.CreateViewport(Picture2.hWnd, "Viewport2")
   Viewport1.SetAutoResize True
   Viewport2.SetAutoResize True
   Viewport1.SetBackgroundColor RGBA(0, 0.5, 0.9, 1)
   Viewport2.SetBackgroundColor RGBA(0, 0.5, 0.9, 1)
   Viewport1.GetCamera.SetViewFrustum 50, 1500
   Viewport2.GetCamera.SetViewFrustum 50, 1500
   Viewport2.GetCamera.SetPosition 0, 0, 0
   
    Set Scene = New TVScene

    Set TextureFactory = New TVTextureFactory

    TextureFactory.LoadTexture "C:\ZaNaZeeN\Mission\Earth.jpg", "LandTexture"
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\Tank.jpg", "TankTexture"
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\Bumb.jpg", "BumbTexture"
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\BumbN.jpg", "BumbNTexture"
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\DB.jpg", "DBTexture"
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\Tent.JPG", "Tent"
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\Object_4.JPG", "Object_4"
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\Tree1.bmp", "Tree1", , , TV_COLORKEY_BLACK
    TextureFactory.LoadTexture "C:\ZaNaZeeN\Textures\Tree2.bmp", "Tree2", , , TV_COLORKEY_BLACK

'Set Atmos = New TVAtmosphere
'Atmos.SkyBox_SetDistance 500
'Atmos.SkyBox_SetTexture GetTex("SkyFront"), GetTex("SkyBack"), GetTex("SkyLeft"), GetTex("SkyRight"), GetTex("SkyTop"), GetTex("SkyBottom")
'Atmos.SkyBox_Enable True

    TV3D.SetAngleSystem TV_ANGLE_DEGREE

    Set TankActor = New TVMesh
    Set TankActor = Scene.CreateMeshBuilder
    
    Set TankActor2 = New TVMesh
    Set TankActor2 = Scene.CreateMeshBuilder
    
    Set TankActorGun = New TVMesh
    Set TankActorGun = Scene.CreateMeshBuilder
    
    Set TankActor2Gun = New TVMesh
    Set TankActor2Gun = Scene.CreateMeshBuilder

For I = 0 To 10
For I2 = 0 To 2
Set Bombing(I, I2) = New TVMesh
Set Bombing(I, I2) = Scene.CreateMeshBuilder
Bombing(I, I2).Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds"
Bombing(I, I2).SetPosition -1000, -1000, -1000
BombingFreeBig(I) = 5
Next
Next

    TankActorGun.Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds"
    TankActorGun.ScaleMesh 0.1, 0.1, 0.1
    TankActorGun.SetPosition 50, 10, 50
    
    TankActor2Gun.Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds"
    TankActor2Gun.ScaleMesh 0.1, 0.1, 0.1
    TankActor2Gun.SetPosition 50, 10, 50
    
    For I = 0 To 19
    Set TankDB1(I) = New TVMesh
    Set TankDB1(I) = Scene.CreateMeshBuilder
    TankDB1(I).Load3DSMesh App.Path + "\Data Files\Models\Ship.3ds"
    TankDB1(I).SetTexture GetTex("DBTexture")
    TankDB1(I).SetPosition 50, -1500, 50
    TankDBH1(I) = 0
    TankDB1(I).SetColor RGBA(1, 0, 0, 1)
    TankDB1(I).ScaleMesh 0.7, 0.7, 0.7
    
    Set TankDB2(I) = New TVMesh
    Set TankDB2(I) = Scene.CreateMeshBuilder
    TankDB2(I).Load3DSMesh App.Path + "\Data Files\Models\Ship.3ds"
    TankDB2(I).SetTexture GetTex("DBTexture")
    TankDB2(I).SetPosition 50, -1500, 50
    TankDBH2(I) = 0
    TankDB2(I).SetColor RGBA(0, 0, 1, 1)
    TankDB2(I).ScaleMesh 0.7, 0.7, 0.7
    Next
    
    P1Gn = 100
    P2Gn = 100
        
    Set Level = New TVMesh

    Set Level = Scene.CreateMeshBuilder
    
    TankActor.Load3DSMesh App.Path + "\Data Files\Models\tank.3ds"
    TankActor.SetTexture GetTex("TankTexture")
    
    TankActor2.Load3DSMesh App.Path + "\Data Files\Models\tank.3ds"
    TankActor2.SetTexture GetTex("TankTexture")
    TankActor2.SetColor RGBA(0, 0, 1, 1)
    
    TankActorH = 100
    TankActor2H = 100
    
    P1Money = 0
    P2Money = 0

Load_Map

    Form1.Show
        
' ßÑÑ ÇáÇÌÑÇÁ ÇáÑÆíÓí | MAIN LOOP
    DoLoop = True
    Main_Loop

End Sub

Private Sub Form_Unload(Cancel As Integer)
    DoLoop = False
    Main_Quit
End Sub

Private Sub Main_Loop()
'Dim SkyRot As Single
TV3D.SetGeneralSpeed 10
Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
'Scene.SetRenderMode TV_LINE
    Do
        DoEvents
Gun_Bombing
'Atmos.SkyBox_SetRotation 0, SkyRot, 0
'SkyRot = SkyRot + 0.1
'If SkyRot > 5000 Then
'SkyRot = 0
'End If

'If Not Snd.Mode = mciModePlay Then
'Snd.Command = "Prev"
'Snd.Command = "Play"
'End If

If Not PWin = 0 Then
If PWinWait < 400 Then
PWinWait = PWinWait + 1

If PWin = 1 Then
Viewport2.GetCamera.SetPosition Tank2PositionF.X - 70, Tank2PositionF.y + 400, Tank2PositionF.z
Viewport2.GetCamera.SetLookAt MKR2.X, MKR2.y, MKR2.z
Label1.Caption = "You Win !"
Label2.Caption = "You Lose !"
Else
Viewport1.GetCamera.SetPosition TankPositionF.X + 70, TankPositionF.y + 400, TankPositionF.z
Viewport1.GetCamera.SetLookAt MKR.X, MKR.y, MKR.z
Label2.Caption = "You Win !"
Label1.Caption = "You Lose !"
End If

GoTo 190
Else
Main_Quit
End If
End If

        Check_Input

        Check_Movement
        
        Check_Players_Coll_Lvl

        Check_Bump_Kill
        
        Check_Dbabat
        
        Check_Move_Gun_Players

' ÇÕØÏÇã ÇááÇÚÈÇä ÇáÑÆíÓíÇä | CHECK IF THE BOTH PLAYERS IF COLLISION
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 35 Then
sngWalk = 0
sng2Walk = 0
sngStrafe = 0
sng2Strafe = 0
TankActor.SetPosition Pl1Pos.X, Pl1Pos.y, Pl1Pos.z
TankPosition = Pl1Pos
TankActor2.SetPosition Pl2Pos.X, Pl2Pos.y, Pl2Pos.z
Tank2Position = Pl2Pos
End If
' ÇÕØÏÇã ÇááÇÚÈÇä ãÚ ÇáãÏÇÝÚ | CHECK IF THE BOTH PLAYERS IF COLLISION WHIT THE BOMBs
For I = 0 To AllTankGActor
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 35 Then
sngWalk = 0
sngStrafe = 0
TankActor.SetPosition Pl1Pos.X, Pl1Pos.y, Pl1Pos.z
TankPosition = Pl1Pos
End If
If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 35 Then
sng2Walk = 0
sng2Strafe = 0
TankActor2.SetPosition Pl2Pos.X, Pl2Pos.y, Pl2Pos.z
Tank2Position = Pl2Pos
End If
Next

190:
' ÇáÑÓã Úáì áæÍÉ ÇááÇÚÈ ÇáÂæá | RENDER TO THE FIRST PLAYER BOARD
    TV3D.SetViewport Viewport1
        TV3D.Clear
        'Atmos.Atmosphere_Render
        Scene.RenderAllMeshes
       For I5 = 0 To AllPpls - 1
        Ppls(I5).Render
       Next
        TV3D.RenderToScreen

' ÇáÑÓã Úáì áæÍÉ ÇááÇÚÈ ÇáËÇäí | RENDER TO THE SECOND PLAYER BOARD
    TV3D.SetViewport Viewport2
        TV3D.Clear
        'Atmos.Atmosphere_Render
        Scene.RenderAllMeshes
       For I5 = 0 To AllPpls - 1
        Ppls(I5).Render
       Next
        TV3D.RenderToScreen

Pl1Pos = TankActor.GetPosition
Pl2Pos = TankActor2.GetPosition

    Loop Until DoLoop = False

    Main_Quit

End Sub

Private Sub Check_Input()
        If InputEngine.IsKeyPressed(TV_KEY_G) = True Then
        If InputEngine.IsKeyPressed(TV_KEY_O) = True Then
        If InputEngine.IsKeyPressed(TV_KEY_D) = True Then
        If InputEngine.IsKeyPressed(TV_KEY_NUMPAD1) = True Then
        TankActorH = 999
        P1Gn = 999
        P1Money = 999
        Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
        End If
        If InputEngine.IsKeyPressed(TV_KEY_NUMPAD2) = True Then
        TankActor2H = 999
        P2Gn = 999
        P2Money = 999
        Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
        End If
        End If
        End If
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_ESCAPE) = True Then
        Main_Quit
        End If
        
        'If InputEngine.IsKeyPressed(TV_KEY_P) = True Then
        'If Not Picture2.Left = 0 Then
        'Picture1.Width = 12000
        'Picture2.Width = 12000
        'Picture2.Left = 0
        'Picture2.Top = 4500
        'Picture1.Height = 4500
        'Picture2.Height = 4500
        'Picture3.Top = 4355
        'Picture4.Top = 4355
        'Picture3.Width = 12000
        'Picture4.Width = 12000
        'Label1.Width = 11900
        'Label2.Width = 11900
        'Else
        'Picture1.Width = 6000
        'Picture2.Width = 6000
        'Picture2.Left = 6000
        'Picture2.Top = 0
        'Picture1.Height = 9000
        'Picture2.Height = 9000
        'Picture3.Top = 8355
        'Picture4.Top = 8355
        'Picture3.Width = 7000
        'Picture4.Width = 7000
        'Label1.Width = 5900
        'Label2.Width = 5900
        'End If
        'End If

' ÊÍÑíß ÇááÇÚÈ ÇáÃæá | MOVE 1 PLAYER
        If InputEngine.IsKeyPressed(TV_KEY_UP) = True Then
            sngWalk = sngWalk + 0.05
            If sngWalk > 1 Then sngWalk = 1
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_DOWN) = True Then
            sngBrake = 0.002
        Else
            sngBrake = 0.0005
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_LEFT) = True Then
            sngAngleY = sngAngleY + 0.1
            If sngStrafe > 1 Then sngStrafe = 1
        ElseIf InputEngine.IsKeyPressed(TV_KEY_RIGHT) = True Then
            sngAngleY = sngAngleY - 0.1
            If sngStrafe < -1 Then sngStrafe = -1
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_RCONTROL) = True Then
            For I = 0 To AllTankGActor
            If TankGActorFC(I) = "N" Then
            If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 100 Then
            TankGActorFC(I) = "1"
            TankGActor(I).SetTexture GetTex("BumbTexture")
            TankGActor(I).SetColor RGBA(1, 1, 1, 1)
            TankGActorG(I).SetColor RGBA(0.1, 0, 0, 1)
            TankGActorH(I) = 20
            P1Money = P1Money + 1
            Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
            End If
            End If
            Next
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_NUMPAD4) = True Then ' ÇÎÊíÇÑ ÇáÓáÇÍ
        If TankDBH1Wait > 0 Then
        TankDBH1Wait = TankDBH1Wait - 1
        GoTo 187
        End If
        If P1GnB = False Then
        P1GnB = True
        Else
        P1GnB = False
        End If
        P1GnW = 0
        Gun1ON = False
        TankActorGun.SetColor RGBA(1, 0, 0, 1)
        TankActorGun.ScaleMesh 0.1, 0.1, 0.1
        TankDBH1Wait = 100
        End If
187:
        
        If InputEngine.IsKeyPressed(TV_KEY_NUMPAD0) = True Then ' ÇØáÇÞ äÇÑ
        If P1Gn > 0 Then
        If Gun1ON = False Then ' ÇÐÇ ÂÎÑ ØáÞÉ ÊãÊ
        Tank1Direction = TankActor.GetPosition
        ' åá åæ ÈÇáÞÑÈ ãä ÇááÇÚÈ ÇáËÇäí
        If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 500 Then
        Tank1Destination = TankActor2.GetPosition
        Gun1Kill = -1
        Gun1ON = True
        If P1GnB = False Then
        P1Gn = P1Gn - 1
        Else
        If P1Gn > 9 Then
        P1Gn = P1Gn - 10
        Else
        P1Gn = 0
        End If
        End If
        Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
        GoTo 196
        End If
        ' åäÇß ãÏÇÝÚ ãÌÇæÑÉ áÞÊÇáåÇ
        For I = 0 To AllTankGActor
        If TankGActorFC(I) = "2" Then
        If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 300 Then
        Tank1Destination = TankGActor(I).GetPosition
        Gun1Kill = I
        Gun1ON = True
        P1Gn = P1Gn - 1
        Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
        GoTo 196
        End If
        End If
        Next
196:
        End If
        End If
        End If

        If InputEngine.IsKeyPressed(TV_KEY_NUMPAD2) = True Then ' äãØ ÇáßÇãíÑÇ
        If ViewModeP1 < 50 Then
        ViewModeP1 = ViewModeP1 + 1
        Else
        ViewModeP1 = 1
        End If
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_NUMPAD1) = True Then ' ÇÓÊÏÚÇÁ ÏÈÇÈÉ
        If P1Money > 0 Then
        If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, MKR.X, 0, MKR.z) < 150 Then
        For I = 0 To 19
        If TankDBH1(I) < 1 Then
        If TankDBH1Wait > 0 Then
        TankDBH1Wait = TankDBH1Wait - 1
        GoTo 194
        End If
        P1Money = P1Money - 1
        TankDBH1Wait = 400
        TankDBH1(I) = 25
        TankDB1(I).SetPosition MKR.X, 15, MKR.z
        TankDB1(I).ScaleMesh 1, 1, 1
        TankDB1Direction(I) = TankDB1(I).GetPosition
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Do While I4 > AllDBDestinationP1 - 1
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Loop
        TankDB1DestinationN(I) = I4
        TankDB1Destination(I) = TankDBDestination(0, TankDB1DestinationN(I))
        TankDB1DirectionNM(I) = 0
        Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
        End If
        Next
        End If
        End If
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_NUMPAD3) = True Then ' ÇÓÊÏÚÇÁ ÏÈÇÈÉ
        If P1Money > 49 Then
        If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, MKR.X, 0, MKR.z) < 150 Then
        For I = 0 To 19
        If TankDBH1(I) < 1 Then
        If TankDBH1Wait > 0 Then
        TankDBH1Wait = TankDBH1Wait - 1
        GoTo 194
        End If
        P1Money = P1Money - 50
        TankDBH1Wait = 400
        TankDBH1(I) = 100
        TankDB1(I).SetPosition MKR.X, 15, MKR.z
        TankDB1(I).ScaleMesh 2, 2, 2
        TankDB1Direction(I) = TankDB1(I).GetPosition
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Do While I4 > AllDBDestinationP1 - 1
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Loop
        TankDB1DestinationN(I) = I4
        TankDB1Destination(I) = TankDBDestination(0, TankDB1DestinationN(I))
        TankDB1DirectionNM(I) = 0
        Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
        End If
        Next
        End If
        End If
        End If
194:
' ÊÍÑíß ÇááÇÚÈ ÇáËÇäí | MOVE 2 PLAYER
        If InputEngine.IsKeyPressed(TV_KEY_W) = True Then
            sng2Walk = sng2Walk + 0.05
            If sng2Walk > 1 Then sng2Walk = 1
        End If

        If InputEngine.IsKeyPressed(TV_KEY_S) = True Then
            sng2Brake = 0.002
        Else
            sng2Brake = 0.0005
        End If

        If InputEngine.IsKeyPressed(TV_KEY_A) = True Then
            sng2AngleY = sng2AngleY + 0.1
            If sng2Strafe > 1 Then sng2Strafe = 1
        ElseIf InputEngine.IsKeyPressed(TV_KEY_D) = True Then
            sng2AngleY = sng2AngleY - 0.1
            If sng2Strafe < -1 Then sng2Strafe = -1
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_LEFTCONTROL) = True Then
            For I = 0 To AllTankGActor
            If TankGActorFC(I) = "N" Then
            If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 100 Then
            TankGActorFC(I) = "2"
            TankGActor(I).SetTexture GetTex("BumbTexture")
            TankGActor(I).SetColor RGBA(0, 0, 1, 1)
            TankGActorG(I).SetColor RGBA(0, 0, 0.1, 1)
            TankGActorH(I) = 20
            P2Money = P2Money + 1
            Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
            End If
            End If
            Next
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_X) = True Then ' ÇÎÊíÇÑ ÇáÓáÇÍ
        If TankDBH2Wait > 0 Then
        TankDBH2Wait = TankDBH2Wait - 1
        GoTo 186
        End If
        If P2GnB = False Then
        P2GnB = True
        Else
        P2GnB = False
        End If
        P2GnW = 0
        Gun2ON = False
        TankActor2Gun.SetColor RGBA(0, 0, 1, 1)
        TankActor2Gun.ScaleMesh 0.1, 0.1, 0.1
        TankDBH2Wait = 100
        End If
186:
        
        If InputEngine.IsKeyPressed(TV_KEY_Q) = True Then
        If P2Gn > 0 Then
        If Gun2ON = False Then ' ÇÐÇ ÂÎÑ ØáÞÉ ÊãÊ
        Tank2Direction = TankActor2.GetPosition
        ' åá åæ ÈÇáÞÑÈ ãä ÇááÇÚÈ ÇáËÇäí
        If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 500 Then
        Tank2Destination = TankActor.GetPosition
        Gun2Kill = -1
        Gun2ON = True
        If P2GnB = False Then
        P2Gn = P2Gn - 1
        Else
        If P2Gn > 9 Then
        P2Gn = P2Gn - 10
        Else
        P2Gn = 0
        End If
        End If
        Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
        GoTo 195
        End If
        ' åäÇß ãÏÇÝÚ ãÌÇæÑÉ áÞÊÇáåÇ
        For I = 0 To AllTankGActor
        If TankGActorFC(I) = "1" Then
        If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 300 Then
        Tank2Destination = TankGActor(I).GetPosition
        Gun2Kill = I
        Gun2ON = True
        P2Gn = P2Gn - 1
        Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
        GoTo 195
        End If
        End If
        Next
195:
        End If
        End If
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_F) = True Then ' äãØ ÇáßÇãíÑÇ
        If ViewModeP2 < 50 Then
        ViewModeP2 = ViewModeP2 + 1
        Else
        ViewModeP2 = 1
        End If
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_E) = True Then ' ÇÓÊÏÚÇÁ ÏÈÇÈÉ
        If P2Money > 0 Then
        If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, MKR2.X, 0, MKR2.z) < 150 Then
        For I = 0 To 19
        If TankDBH2(I) < 1 Then
        If TankDBH2Wait > 0 Then
        TankDBH2Wait = TankDBH2Wait - 1
        GoTo 193
        End If
        P2Money = P2Money - 1
        TankDBH2Wait = 400
        TankDBH2(I) = 25
        TankDB2(I).SetPosition MKR2.X, 15, MKR2.z
        TankDB2(I).ScaleMesh 1, 1, 1
        TankDB2Direction(I) = TankDB2(I).GetPosition
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Do While I4 > AllDBDestinationP2 - 1
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Loop
        TankDB2DestinationN(I) = I4
        TankDB2Destination(I) = TankDBDestination2(0, TankDB2DestinationN(I))
        TankDB2DirectionNM(I) = 0
        Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
        End If
        Next
        End If
        End If
        End If
        
        If InputEngine.IsKeyPressed(TV_KEY_Z) = True Then ' ÇÓÊÏÚÇÁ ÏÈÇÈÉ
        If P2Money > 49 Then
        If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, MKR2.X, 0, MKR2.z) < 150 Then
        For I = 0 To 19
        If TankDBH2(I) < 1 Then
        If TankDBH2Wait > 0 Then
        TankDBH2Wait = TankDBH2Wait - 1
        GoTo 193
        End If
        P2Money = P2Money - 50
        TankDBH2Wait = 400
        TankDBH2(I) = 100
        TankDB2(I).SetPosition MKR2.X, 15, MKR2.z
        TankDB2(I).ScaleMesh 2, 2, 2
        TankDB2Direction(I) = TankDB2(I).GetPosition
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Do While I4 > AllDBDestinationP2 - 1
        X0 = Right(Rnd, 1)
        I4 = Int(X0)
        Loop
        TankDB2DestinationN(I) = I4
        TankDB2Destination(I) = TankDBDestination2(0, TankDB2DestinationN(I))
        TankDB2DirectionNM(I) = 0
        Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
        End If
        Next
        End If
        End If
        End If
193:
End Sub

Private Sub Check_Movement()
' ÊÍÑíß ÇáäÇÓ Ýí ÇáãÏíäÉ
For I5 = 0 To AllPpls - 1
If PplsDie(I5) = True Then ' ÇÐÇ ãíÊ íÌÊÇÒå
GoTo 45
End If
' íäÝÚÓ ÈÏÈÇÈÉ ÇááÇÚÈÇä
If GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 20 Or GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 20 Then
If PplsDie(I5) = False Then
PplsDie(I5) = True
PplsGif(I5) = "Die"
Ppls(I5).SetAnimationLoop False
If PplsDir(I5) = "D" Then
Ppls(I5).SetAnimationName "diesimple"
End If
If PplsDir(I5) = "U" Then
Ppls(I5).SetAnimationName "dieforward"
End If
If PplsDir(I5) = "L" Then
Ppls(I5).SetAnimationName "dieforward1"
End If
If PplsDir(I5) = "R" Then
Ppls(I5).SetAnimationName "diebackward"
End If
End If
GoTo 45
End If
' íÊÍÑß ÍÑßÉ ãÇ ßá ÝÊÑÉ
If PplsGif(I5) = "walk" And Not PplsGif(I5) = "Die" Then
If Rnd > 0.99 Then
PplsGif(I5) = "Move"
PplsLastFrame(I5) = 0
I4 = Int(Rnd * 10)
Select Case I4
Case Is = 0: Ppls(I5).SetAnimationName "tieshoe": PplsLastFrame(I5) = 50
Case Is = 1: Ppls(I5).SetAnimationName "lean": PplsLastFrame(I5) = 49
Case Is = 2: Ppls(I5).SetAnimationName "pondering": PplsLastFrame(I5) = 34
Case Is = 3: Ppls(I5).SetAnimationName "pondering2": PplsLastFrame(I5) = 69
Case Is = 4: Ppls(I5).SetAnimationName "pondering3": PplsLastFrame(I5) = 34
Case Is = 5: Ppls(I5).SetAnimationName "startle": PplsLastFrame(I5) = 15
Case Is = 6: Ppls(I5).SetAnimationName "c1a0_catwalkidle": PplsLastFrame(I5) = 49
Case Is = 7: Ppls(I5).SetAnimationName "tieshoe": PplsLastFrame(I5) = 50
Case Is = 8: Ppls(I5).SetAnimationName "onguard": PplsLastFrame(I5) = 49
Case Is = 9: Ppls(I5).SetAnimationName "quicklook": PplsLastFrame(I5) = 44
End Select
Ppls(I5).SetAnimationLoop False
End If
Else
If PplsGif(I5) = "Move" Then
If Int(Ppls(I5).GetFrame) = PplsLastFrame(I5) Then
PplsGif(I5) = "walk"
Ppls(I5).SetAnimationName "walk"
Ppls(I5).SetAnimationLoop True
End If
End If
End If
' ÍÑßÉ ÇáÔÎÕ
If PplsDir(I5) = "D" Then
If GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z + 1, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 50 Or GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z + 1, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 50 Then
If Not PplsGif(I5) = "flinch" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "flinch"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "flinch"
End If
If PplsFrid(I5) < 50000 Then
PplsFrid(I5) = PplsFrid(I5) + 1
End If
GoTo 45
End If
If PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "run"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "run"
End If
PplsFrid(I5) = PplsFrid(I5) - 1
Else
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
End If
I3 = -1
For I4 = 0 To AllLvlColl - 1
If Not PplsFrid(I5) > 0 Then
If GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z + 1, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 And GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z + 1, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) > 50 Then
I3 = I3 + 1
End If
Else
If GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z + 3, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 And GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z + 1, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) > 50 Then
I3 = I3 + 1
End If
End If
Next
If I3 = AllLvlColl - 1 Then
If Not PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z + 1
End If
Else
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z + 3
End If
End If
Ppls(I5).SetRotation 0, 90, 0
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
GoTo 45
Else
GoTo 46
End If
End If
If PplsDir(I5) = "U" Then
If GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z - 1, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 50 Or GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z - 1, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 50 Then
If Not PplsGif(I5) = "flinch" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "crouch_idle3"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "flinch"
End If
If PplsFrid(I5) < 50000 Then
PplsFrid(I5) = PplsFrid(I5) + 1
End If
GoTo 45
End If
If PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "run2"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "run"
End If
PplsFrid(I5) = PplsFrid(I5) - 1
Else
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
End If
I3 = -1
For I4 = 0 To AllLvlColl - 1
If Not PplsFrid(I5) > 0 Then
If GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z - 1, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 And GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z - 1, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) > 50 Then
I3 = I3 + 1
End If
Else
If GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z - 3, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 And GetDistance3D(Ppls(I5).GetPosition.X, 0, Ppls(I5).GetPosition.z - 1, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) > 50 Then
I3 = I3 + 1
End If
End If
Next
If I3 = AllLvlColl - 1 Then
If Not PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z - 1
End If
Else
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z - 3
End If
End If
Ppls(I5).SetRotation 0, -90, 0
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
GoTo 45
Else
GoTo 46
End If
End If
If PplsDir(I5) = "R" Then
If GetDistance3D(Ppls(I5).GetPosition.X + 1, 0, Ppls(I5).GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 50 Or GetDistance3D(Ppls(I5).GetPosition.X + 1, 0, Ppls(I5).GetPosition.z, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 50 Then
If Not PplsGif(I5) = "flinch" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "fear2"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "flinch"
End If
If PplsFrid(I5) < 50000 Then
PplsFrid(I5) = PplsFrid(I5) + 1
End If
GoTo 45
End If
If PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "run1"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "run"
End If
PplsFrid(I5) = PplsFrid(I5) - 1
Else
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
End If
I3 = -1
For I4 = 0 To AllLvlColl - 1
If Not PplsFrid(I5) > 0 Then
If GetDistance3D(Ppls(I5).GetPosition.X + 1, 0, Ppls(I5).GetPosition.z, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 And GetDistance3D(Ppls(I5).GetPosition.X + 1, 0, Ppls(I5).GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) > 50 Then
I3 = I3 + 1
End If
Else
If GetDistance3D(Ppls(I5).GetPosition.X + 3, 0, Ppls(I5).GetPosition.z, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 And GetDistance3D(Ppls(I5).GetPosition.X + 1, 0, Ppls(I5).GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) > 50 Then
I3 = I3 + 1
End If
End If
Next
If I3 = AllLvlColl - 1 Then
If Not PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X + 1, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z
End If
Else
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X + 3, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z
End If
End If
Ppls(I5).SetRotation 0, 180, 0
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
GoTo 45
Else
GoTo 46
End If
End If
If PplsDir(I5) = "L" Then
If GetDistance3D(Ppls(I5).GetPosition.X - 1, 0, Ppls(I5).GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 50 Or GetDistance3D(Ppls(I5).GetPosition.X - 1, 0, Ppls(I5).GetPosition.z, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 50 Then
If Not PplsGif(I5) = "flinch" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "panic"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "flinch"
End If
If PplsFrid(I5) < 50000 Then
PplsFrid(I5) = PplsFrid(I5) + 1
End If
GoTo 45
End If
If PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" Then
Ppls(I5).SetAnimationName "run"
If PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationLoop True
End If
PplsGif(I5) = "run"
End If
PplsFrid(I5) = PplsFrid(I5) - 1
Else
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
End If
I3 = -1
For I4 = 0 To AllLvlColl - 1
If Not PplsFrid(I5) > 0 Then
If GetDistance3D(Ppls(I5).GetPosition.X - 1, 0, Ppls(I5).GetPosition.z, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 Then
I3 = I3 + 1
End If
Else
If GetDistance3D(Ppls(I5).GetPosition.X - 3, 0, Ppls(I5).GetPosition.z, LvlColl(I4).X, 0, LvlColl(I4).z) > 70 Then
I3 = I3 + 1
End If
End If
Next
If I3 = AllLvlColl - 1 Then
If Not PplsFrid(I5) > 0 Then
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X - 1, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z
End If
Else
If Not PplsGif(I5) = "Move" Then
Ppls(I5).SetPosition Ppls(I5).GetPosition.X - 3, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z
End If
End If
Ppls(I5).SetRotation 0, 0, 0
If Not PplsGif(I5) = "walk" And Not PplsGif(I5) = "run" And Not PplsGif(I5) = "Die" And Not PplsGif(I5) = "Move" Then
Ppls(I5).SetAnimationName "walk"
PplsGif(I5) = "walk"
End If
GoTo 45
Else
GoTo 46
End If
End If
46:
' ÇáÔÎÕ ÓíÏíÑ æÌåå ÈÓÈÈ æÌæÏ ÚÇÆÞ
If PplsDir(I5) = "L" Then
Select Case Int(Rnd * 10)
Case Is = 0 Or 1 Or 2 Or 3: PplsDir(I5) = "R"
Case Is = 4 Or 5 Or 6: PplsDir(I5) = "U"
Case Is = 7 Or 8 Or 9: PplsDir(I5) = "D"
End Select
Ppls(I5).SetPosition Ppls(I5).GetPosition.X + 3, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z
GoTo 45
End If
If PplsDir(I5) = "R" Then
Select Case Int(Rnd * 10)
Case Is = 0 Or 1 Or 2 Or 3: PplsDir(I5) = "L"
Case Is = 6 Or 5 Or 4: PplsDir(I5) = "D"
Case Is = 7 Or 8 Or 9: PplsDir(I5) = "U"
End Select
Ppls(I5).SetPosition Ppls(I5).GetPosition.X - 3, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z
GoTo 45
End If
If PplsDir(I5) = "U" Then
Select Case Int(Rnd * 10)
Case Is = 0 Or 1 Or 2 Or 3: PplsDir(I5) = "D"
Case Is = 4 Or 5 Or 6: PplsDir(I5) = "L"
Case Is = 7 Or 8 Or 9: PplsDir(I5) = "R"
End Select
Ppls(I5).SetPosition Ppls(I5).GetPosition.X, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z + 3
GoTo 45
End If
If PplsDir(I5) = "D" Then
Select Case Int(Rnd * 10)
Case Is = 0 Or 1 Or 2 Or 3: PplsDir(I5) = "U"
Case Is = 4 Or 5 Or 6: PplsDir(I5) = "R"
Case Is = 7 Or 8 Or 9: PplsDir(I5) = "L"
End Select
Ppls(I5).SetPosition Ppls(I5).GetPosition.X, Ppls(I5).GetPosition.y, Ppls(I5).GetPosition.z - 3
GoTo 45
End If
45:
Next

' ÊÍÑíß ÇááÇÚÈ ÇáÃæá | MOVE PLAYER 1
        Select Case sngWalk
        Case Is > 0
            sngWalk = sngWalk - sngBrake * TV3D.TimeElapsed
            If sngWalk < 0 Then sngWalk = 0
        End Select
        
        Select Case sngStrafe
        Case Is > 0
            sngStrafe = sngStrafe - 0.001 * TV3D.TimeElapsed
            If sngStrafe < 0 Then sngStrafe = 0
        Case Is < 0
            sngStrafe = sngStrafe + 0.001 * TV3D.TimeElapsed
            If sngStrafe > 0 Then sngStrafe = 0
        End Select
        
' ÊÍÑíß ÇááÇÚÈ ÇáËÇäí | MOVE PLAYER 2
        Select Case sng2Walk
        Case Is > 0
            sng2Walk = sng2Walk - sng2Brake * TV3D.TimeElapsed
            If sng2Walk < 0 Then sng2Walk = 0
        End Select

        Select Case sng2Strafe
        Case Is > 0
            sng2Strafe = sng2Strafe - 0.001 * TV3D.TimeElapsed
            If sng2Strafe < 0 Then sng2Strafe = 0
        Case Is < 0
            sng2Strafe = sng2Strafe + 0.001 * TV3D.TimeElapsed
            If sng2Strafe > 0 Then sng2Strafe = 0
        End Select

' ÊÍÏíÏ ÇáãæÇÖÚ | REFRESH POS.
        TankPosition.X = TankPosition.X + (Cos(sngAngleY) * sngWalk / 2 * TV3D.TimeElapsed) + (Cos(sngAngleY + 3.141596 / 2) * sngStrafe / 5 * TV3D.TimeElapsed)
        TankPosition.z = TankPosition.z + (Sin(sngAngleY) * sngWalk / 2 * TV3D.TimeElapsed) + (Sin(sngAngleY + 3.141596 / 2) * sngStrafe / 5 * TV3D.TimeElapsed)
        TankPosition.y = 10
        
        Tank2Position.X = Tank2Position.X + (Cos(sng2AngleY) * sng2Walk / 2 * TV3D.TimeElapsed) + (Cos(sng2AngleY + 3.141596 / 2) * sng2Strafe / 5 * TV3D.TimeElapsed)
        Tank2Position.z = Tank2Position.z + (Sin(sng2AngleY) * sng2Walk / 2 * TV3D.TimeElapsed) + (Sin(sng2AngleY + 3.141596 / 2) * sng2Strafe / 5 * TV3D.TimeElapsed)
        Tank2Position.y = 10

        TankActor.SetPosition TankPosition.X, TankPosition.y, TankPosition.z
        TankActor2.SetPosition Tank2Position.X, Tank2Position.y, Tank2Position.z

        TankActor.SetRotation -90 + (sngStrafe * 10), -90 + (sngAngleY * -57.295), 0
        TankActor2.SetRotation -90 + (sng2Strafe * 10), -90 + (sng2AngleY * -57.295), 0
        
' ÇáßÇãíÑÇ | THE CAMERA
        Dim tmpLookAtX As Single, tmpLookAtZ As Single, tmpLookAtY As Single
        tmpLookAtX = TankPosition.X
        tmpLookAtZ = TankPosition.z
        tmpLookAtY = 30

Select Case ViewModeP1
Case Is > 40:
Viewport1.GetCamera.SetPosition TankPosition.X - 70, TankPosition.y + 400, TankPosition.z
Case Is > 30:
Viewport1.GetCamera.SetPosition TankPosition.X - 70, TankPosition.y + 80, TankPosition.z - 70
Case Is > 20:
Viewport1.GetCamera.SetPosition TankPosition.X - 1, TankPosition.y + 500, TankPosition.z - 1
Case Is > 10:
Viewport1.GetCamera.SetPosition TankPosition.X + 70, TankPosition.y + 40, TankPosition.z + 70
Case Is > -1:
Viewport1.GetCamera.SetPosition TankPosition.X, TankPosition.y + 10.5 - (sngWalk * 1.7), TankPosition.z
Viewport1.GetCamera.SetRotation (sngWalk * 14), -(sngAngleY * 57.3) + 90, 0
GoTo 3647
End Select
Viewport1.GetCamera.SetLookAt tmpLookAtX, tmpLookAtY, tmpLookAtZ
3647:

tmpLookAtX = Tank2Position.X
tmpLookAtZ = Tank2Position.z
Select Case ViewModeP2
Case Is > 40:
Viewport2.GetCamera.SetPosition Tank2Position.X + 70, Tank2Position.y + 400, Tank2Position.z
Case Is > 30:
Viewport2.GetCamera.SetPosition Tank2Position.X + 70, Tank2Position.y + 80, Tank2Position.z + 70
Case Is > 20:
Viewport2.GetCamera.SetPosition Tank2Position.X + 1, Tank2Position.y + 500, Tank2Position.z + 1
Case Is > 10:
Viewport2.GetCamera.SetPosition Tank2Position.X - 70, Tank2Position.y + 40, Tank2Position.z - 70
Case Is > -1:
Viewport2.GetCamera.SetPosition Tank2Position.X, Tank2Position.y + 10.5 - (sng2Walk * 1.7), Tank2Position.z
Viewport2.GetCamera.SetRotation (sng2Walk * 14), -(sng2AngleY * 57.3) + 90, 0
GoTo 36472
End Select
Viewport2.GetCamera.SetLookAt tmpLookAtX, tmpLookAtY, tmpLookAtZ
36472:
End Sub

Private Sub Main_Quit()
    Set TextureFactory = Nothing
    Set InputEngine = Nothing
    Set Scene = Nothing
    Set TV3D = Nothing
    End
End Sub

Private Sub Load_Map()
Level.AddFloor GetTex("LandTexture"), -1000, -1000, 4000, 4000, -1
'Snd.FileName = "C:\ZaNaZeeN\Mission\Music.wav"
'Snd.Command = "Open"
'DoEvents

Open "C:\ZaNaZeeN\Mission\Map.Dll" For Input As #1
Input #1, I2
For I = 1 To I2
Input #1, X0
If Left(X0, 1) = "1" Then
If Not Left(X0, 6) = "1_Tree" Then
TextureFactory.LoadTexture "C:\ZaNaZeeN\Mission\" + X0, X0, , , TV_COLORKEY_WHITE
Else
TextureFactory.LoadTexture "C:\ZaNaZeeN\Mission\" + X0, X0, , , TV_COLORKEY_BLACK
End If
Else
TextureFactory.LoadTexture "C:\ZaNaZeeN\Mission\" + X0, X0
End If
Next

Input #1, I2
Input #1, I3
For I = 0 To I2 - 1
For I4 = 0 To I3 - 1
    Input #1, X0
    Input #1, X1
If X0 = " " Then
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
GoTo 190
End If

If X0 = "PPL" Then
Set Ppls(AllPpls) = New TVActor
If LastPplBuild = "" Or LastPplBuild = "U" Then
PplsDir(AllPpls) = "U"
LastPplBuild = "R"
GoTo 146813
End If
If LastPplBuild = "R" Then
PplsDir(AllPpls) = "R"
LastPplBuild = "D"
GoTo 146813
End If
If LastPplBuild = "D" Then
PplsDir(AllPpls) = "D"
LastPplBuild = "L"
GoTo 146813
End If
If LastPplBuild = "L" Then
PplsDir(AllPpls) = "L"
LastPplBuild = "U"
GoTo 146813
End If
146813:
PplsGT(AllPpls) = Vector((I * 100) + 50, 0, (I4 * 100) + 150)
Ppls(AllPpls).Load App.Path + "\Data Files\Models\scientist.mdl"
Ppls(AllPpls).SetAnimationLoop True
Ppls(AllPpls).SetAnimationName "walk"
PplsGif(I5) = "walk"
Ppls(AllPpls).SetRotation 0, 90, 0
Ppls(AllPpls).SetPosition (I * 100) + 50, 0, (I4 * 100) + 50
Ppls(AllPpls).SetScale 0.4, 0.4, 0.4
Ppls(AllPpls).SetTexture GetTex("Nothing")
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_Back.Jpg", "PP0"
Ppls(AllPpls).SetTexture GetTex("PP0"), 0
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_L_Leg.Jpg", "PP1"
Ppls(AllPpls).SetTexture GetTex("PP1"), 1
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_Foot.Jpg", "PP2"
Ppls(AllPpls).SetTexture GetTex("PP2"), 2
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_Front.Jpg", "PP3"
Ppls(AllPpls).SetTexture GetTex("PP3"), 3
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_Jens.Jpg", "PP4"
Ppls(AllPpls).SetTexture GetTex("PP4"), 4
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_JensB.Jpg", "PP5"
Ppls(AllPpls).SetTexture GetTex("PP5"), 5
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_B_Legs.Jpg", "PP6"
Ppls(AllPpls).SetTexture GetTex("PP6"), 6
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_Appe.Jpg", "PP7"
Ppls(AllPpls).SetTexture GetTex("PP7"), 7
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_R_Leg.Jpg", "PP8"
Ppls(AllPpls).SetTexture GetTex("PP8"), 8
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_L_Hand.Jpg", "PP9"
Ppls(AllPpls).SetTexture GetTex("PP9"), 9
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_Hands.Jpg", "PP10"
Ppls(AllPpls).SetTexture GetTex("PP10"), 10
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_F_Hands.Jpg", "PP11"
Ppls(AllPpls).SetTexture GetTex("PP11"), 11
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_R_Hand.Jpg", "PP12"
Ppls(AllPpls).SetTexture GetTex("PP12"), 12
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_B_Face.Jpg", "PP13"
Ppls(AllPpls).SetTexture GetTex("PP13"), 13
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_F_Face.Jpg", "PP14"
Ppls(AllPpls).SetTexture GetTex("PP14"), 14
Ppls(AllPpls).SetTexture GetTex("PP10"), 15
Ppls(AllPpls).SetTexture GetTex("PP10"), 16
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_Watcher.Jpg", "PP17"
Ppls(AllPpls).SetTexture GetTex("PP17"), 17
TextureFactory.LoadTexture App.Path + "\Data Files\Models\ppl_Tex_X_Watcher.Jpg", "PP18"
Ppls(AllPpls).SetTexture GetTex("PP18"), 18
AllPpls = AllPpls + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
GoTo 190
End If

If X0 = "G" Then
    Set TankGActor(AllTankGActor) = New TVMesh
    Set TankGActor(AllTankGActor) = Scene.CreateMeshBuilder
    TankGActor(AllTankGActor).Load3DSMesh App.Path + "\Data Files\Models\Bumb.3ds"
    TankGActor(AllTankGActor).SetTexture GetTex("BumbNTexture")
    TankGActor(AllTankGActor).SetPosition (I * 100) + 50, 10, (I4 * 100) + 50
    TankGActor(AllTankGActor).SetRotation -90, -90, 0
    TankGActorFC(AllTankGActor) = "N"
    
    Set TankGActorG(AllTankGActor) = New TVMesh
    Set TankGActorG(AllTankGActor) = Scene.CreateMeshBuilder
    TankGActorG(AllTankGActor).Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds"
    TankGActorG(AllTankGActor).SetColor RGBA(0, 0.1, 0, 1)
    TankGActorG(AllTankGActor).ScaleMesh 0.1, 0.1, 0.1
    TankGActorG(AllTankGActor).SetPosition (I * 100) + 50, 30, (I4 * 100) + 50
    TankGActorH(AllTankGActor) = 20
    AllTankGActor = AllTankGActor + 1
    Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
GoTo 190
End If
If X0 = "J" Then
    Set TankGActor(AllTankGActor) = New TVMesh
    Set TankGActor(AllTankGActor) = Scene.CreateMeshBuilder
    TankGActor(AllTankGActor).Load3DSMesh App.Path + "\Data Files\Models\Bumb.3ds"
    TankGActor(AllTankGActor).SetTexture GetTex("BumbNTexture")
    TankGActor(AllTankGActor).SetPosition (I * 100) + 50, 110, (I4 * 100) + 50
    TankGActor(AllTankGActor).SetRotation -90, -90, 0
    TankGActorFC(AllTankGActor) = "N"
    
    Set TankGActorG(AllTankGActor) = New TVMesh
    Set TankGActorG(AllTankGActor) = Scene.CreateMeshBuilder
    TankGActorG(AllTankGActor).Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds"
    TankGActorG(AllTankGActor).SetColor RGBA(0, 0.1, 0, 1)
    TankGActorG(AllTankGActor).ScaleMesh 0.1, 0.1, 0.1
    TankGActorG(AllTankGActor).SetPosition (I * 100) + 50, 130, (I4 * 100) + 50
    TankGActorH(AllTankGActor) = 20
    AllTankGActor = AllTankGActor + 1
    Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0

Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 100
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 100
Level.AddWall GetTex(X1), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddWall GetTex(X1), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 75
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "K" Then
    Set TankGActor(AllTankGActor) = New TVMesh
    Set TankGActor(AllTankGActor) = Scene.CreateMeshBuilder
    TankGActor(AllTankGActor).Load3DSMesh App.Path + "\Data Files\Models\Bumb.3ds"
    TankGActor(AllTankGActor).SetTexture GetTex("BumbNTexture")
    TankGActor(AllTankGActor).SetPosition (I * 100) + 50, 60, (I4 * 100) + 50
    TankGActor(AllTankGActor).SetRotation -90, -90, 0
    TankGActorFC(AllTankGActor) = "N"
    
    Set TankGActorG(AllTankGActor) = New TVMesh
    Set TankGActorG(AllTankGActor) = Scene.CreateMeshBuilder
    TankGActorG(AllTankGActor).Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds"
    TankGActorG(AllTankGActor).SetColor RGBA(0, 0.1, 0, 1)
    TankGActorG(AllTankGActor).ScaleMesh 0.1, 0.1, 0.1
    TankGActorG(AllTankGActor).SetPosition (I * 100) + 50, 80, (I4 * 100) + 50
    TankGActorH(AllTankGActor) = 20
    AllTankGActor = AllTankGActor + 1
    Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 50
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 50
Level.AddWall GetTex(X1), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 50
Level.AddWall GetTex(X1), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 50
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 50
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 75
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "M" Then
Level.AddWall GetTex(X1 + "L" + ".Bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 100
Level.AddWall GetTex(X1 + "D" + ".Bmp"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 100
Level.AddWall GetTex(X1 + "U" + ".Bmp"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddWall GetTex(X1 + "R" + ".Bmp"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddFloor GetTex(X1 + "T" + ".Bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddFloor GetTex(X1 + "F" + ".Bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
MKR.X = (I * 100) + 50
MKR.z = (I4 * 100) + 50
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "N" Then
Level.AddWall GetTex(X1 + "L" + ".Bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 100
Level.AddWall GetTex(X1 + "U" + ".Bmp"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 100
Level.AddWall GetTex(X1 + "D" + ".Bmp"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddWall GetTex(X1 + "R" + ".Bmp"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddFloor GetTex(X1 + "T" + ".Bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddFloor GetTex(X1 + "F" + ".Bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
MKR2.X = (I * 100) + 50
MKR2.z = (I4 * 100) + 50
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "B" Then
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 100
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 100
Level.AddWall GetTex(X1), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddWall GetTex(X1), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "P" Then
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 50
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 50
Level.AddWall GetTex(X1), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 50
Level.AddWall GetTex(X1), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 50
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 50
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "A" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_1.3DS"
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 10, (I4 * 100) + 50
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 19
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "C" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_2.3DS"
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 14, (I4 * 100) + 50
LvlMesh(AllLvlMesh).ScaleMesh 0.4, 0.4, 0.4
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 30
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "D" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_3.3DS"
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 25, (I4 * 100) + 50
LvlMesh(AllLvlMesh).ScaleMesh 0.57, 0.57, 0.57
LvlMesh(AllLvlMesh).SetTexture GetTex("Tent")
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 55
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "E" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_4.3DS"
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 17, (I4 * 100) + 50
LvlMesh(AllLvlMesh).SetTexture GetTex("Object_4")
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 55
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "F" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_5.3DS"
LvlMesh(AllLvlMesh).ScaleMesh 0.4, 0.4, 0.4
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 12, (I4 * 100) + 50
LvlMesh(AllLvlMesh).SetTexture GetTex("Tent")
LvlMesh(AllLvlMesh).SetRotation 0, 45, 0
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 40
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "G" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_6.3DS"
LvlMesh(AllLvlMesh).ScaleMesh 0.4, 0.4, 0.4
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 12, (I4 * 100) + 50
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 40
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "H" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_7.3DS"
LvlMesh(AllLvlMesh).ScaleMesh 2, 2, 2
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 15, (I4 * 100) + 50
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 55
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "I" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Object_8.3DS"
LvlMesh(AllLvlMesh).ScaleMesh 0.5, 0.5, 0.5
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 15, (I4 * 100) + 50
LvlMesh(AllLvlMesh).SetTexture GetTex("Tent")
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 38
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "L" Then
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
Level.AddWall GetTex("0_JK.JPG"), (I * 100) + 30, (I4 * 100) + 40, (I * 100) + 70, (I4 * 100) + 40, 150
Level.AddWall GetTex("0_JK.JPG"), (I * 100) + 30, (I4 * 100) + 60, (I * 100) + 70, (I4 * 100) + 60, 150
Level.AddWall GetTex("0_JK.JPG"), (I * 100) + 30, (I4 * 100) + 40, (I * 100) + 30, (I4 * 100) + 60, 150
Level.AddWall GetTex("0_JK.JPG"), (I * 100) + 70, (I4 * 100) + 40, (I * 100) + 70, (I4 * 100) + 60, 150
Level.AddFloor GetTex("0_JK.JPG"), (I * 100) + 10, (I4 * 100) + 20, (I * 100) + 90, (I4 * 100) + 80, 121
Level.AddFloor GetTex("0_JK.JPG"), (I * 100) + 10, (I4 * 100) + 20, (I * 100) + 90, (I4 * 100) + 80, 124
Level.AddFloor GetTex("0_JK.JPG"), (I * 100) + 10, (I4 * 100) + 20, (I * 100) + 90, (I4 * 100) + 80, 127
Level.AddFloor GetTex("0_JK.JPG"), (I * 100) + 20, (I4 * 100) + 30, (I * 100) + 80, (I4 * 100) + 70, 130
Level.AddFloor GetTex("0_JK.JPG"), (I * 100) + 20, (I4 * 100) + 30, (I * 100) + 80, (I4 * 100) + 70, 135
Level.AddFloor GetTex("0_JK.JPG"), (I * 100) + 20, (I4 * 100) + 30, (I * 100) + 80, (I4 * 100) + 70, 140
Level.AddFloor GetTex("0_JK.JPG"), (I * 100) + 30, (I4 * 100) + 40, (I * 100) + 70, (I4 * 100) + 60, 150
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 40
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "O" Then
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100), 0, (I4 * 100), (I * 100) + 50, 100, (I4 * 100) + 50, (I * 100) + 100, 0, (I4 * 100), (I * 100) + 50, 100, (I4 * 100) + 50
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100), 0, (I4 * 100), (I * 100) + 50, 100, (I4 * 100) + 50, (I * 100), 0, (I4 * 100) + 100, (I * 100) + 50, 100, (I4 * 100) + 50
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100) + 100, 0, (I4 * 100), (I * 100) + 50, 100, (I4 * 100) + 50, (I * 100) + 100, 0, (I4 * 100) + 100, (I * 100) + 50, 100, (I4 * 100) + 50
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100), 0, (I4 * 100) + 100, (I * 100) + 50, 100, (I4 * 100) + 50, (I * 100) + 100, 0, (I4 * 100) + 100, (I * 100) + 50, 100, (I4 * 100) + 50
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 75
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "Q" Then
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100), 0, (I4 * 100), (I * 100) + 150, 100, (I4 * 100) + 150, (I * 100) + 300, 0, (I4 * 100), (I * 100) + 150, 300, (I4 * 100) + 150
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100), 0, (I4 * 100), (I * 100) + 150, 300, (I4 * 100) + 150, (I * 100), 0, (I4 * 100) + 300, (I * 100) + 150, 300, (I4 * 100) + 150
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100) + 300, 0, (I4 * 100), (I * 100) + 150, 300, (I4 * 100) + 150, (I * 100) + 300, 0, (I4 * 100) + 300, (I * 100) + 150, 300, (I4 * 100) + 150
Level.AddFaceFromPoint GetTex("0_HRM.JPG"), (I * 100), 0, (I4 * 100) + 300, (I * 100) + 150, 300, (I4 * 100) + 150, (I * 100) + 300, 0, (I4 * 100) + 300, (I * 100) + 150, 300, (I4 * 100) + 150
LvlColl(AllLvlColl).X = (I * 100) + 150
LvlColl(AllLvlColl).z = (I4 * 100) + 150
LvlColl(AllLvlColl).y = 215
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "R" Then
Level.AddFloor GetTex("1_Tree_Fl_1.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 200
Level.AddWall GetTex(X1), (I * 100) + 100, (I4 * 100), (I * 100), (I4 * 100) + 100, 200
Level.AddWall GetTex(X1), (I * 100), (I4 * 100) + 50, (I * 100) + 100, (I4 * 100) + 50, 200
Level.AddWall GetTex(X1), (I * 100) + 50, (I4 * 100) + 100, (I * 100) + 50, (I4 * 100) + 100, 200
Level.AddFloor GetTex("1_Tree_Top2.bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 150
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 10
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "S" Then
Level.AddFloor GetTex("1_Tree_Fl_1.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
Level.AddWall GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 120
Level.AddWall GetTex(X1), (I * 100) + 100, (I4 * 100), (I * 100), (I4 * 100) + 100, 120
Level.AddWall GetTex(X1), (I * 100), (I4 * 100) + 50, (I * 100) + 100, (I4 * 100) + 50, 120
Level.AddWall GetTex(X1), (I * 100) + 50, (I4 * 100) + 100, (I * 100) + 50, (I4 * 100) + 100, 120
Level.AddFloor GetTex("1_Tree_Top.bmp"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 70
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 10
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "T" Then
Level.AddWall GetTex(X1 + "_F.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 100
Level.AddWall GetTex(X1 + "_F.JPG"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 100
Level.AddWall GetTex(X1 + "_F.JPG"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddWall GetTex(X1 + "_F.JPG"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 100
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 100
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UL" Then
Level.AddWall GetTex(X1 + "_L.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UU" Then
Level.AddWall GetTex(X1 + "_U.JPG"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UD" Then
Level.AddWall GetTex(X1 + "_D.JPG"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UR" Then
Level.AddWall GetTex(X1 + "_R.JPG"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UUL" Then
Level.AddWall GetTex(X1 + "_U.JPG"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 300
Level.AddWall GetTex(X1 + "_L.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UUR" Then
Level.AddWall GetTex(X1 + "_U.JPG"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 300
Level.AddWall GetTex(X1 + "_R.JPG"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UDR" Then
Level.AddWall GetTex(X1 + "_D.JPG"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddWall GetTex(X1 + "_R.JPG"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UDL" Then
Level.AddWall GetTex(X1 + "_D.JPG"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddWall GetTex(X1 + "_L.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UUD" Then
Level.AddWall GetTex(X1 + "_D.JPG"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddWall GetTex(X1 + "_U.JPG"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "URL" Then
Level.AddWall GetTex(X1 + "_L.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 300
Level.AddWall GetTex(X1 + "_R.JPG"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UUDRL" Then
Level.AddWall GetTex(X1 + "_D.JPG"), (I * 100) + 100, (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddWall GetTex(X1 + "_U.JPG"), (I * 100), (I4 * 100), (I * 100), (I4 * 100) + 100, 300
Level.AddWall GetTex(X1 + "_L.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100), 300
Level.AddWall GetTex(X1 + "_R.JPG"), (I * 100), (I4 * 100) + 100, (I * 100) + 100, (I4 * 100) + 100, 300
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 77
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "UT" Then
Level.AddFloor GetTex(X1 + "_T.JPG"), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 300
GoTo 190
End If
If X0 = "V" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Tree1.3ds", False, False, False, False, False
LvlMesh(AllLvlMesh).ScaleMesh 5, 5, 5
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 50, 0, (I4 * 100) + 85
LvlMesh(AllLvlMesh).SetTexture GetTex("Tree1")
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
GoTo 190
End If
If X0 = "W" Then
Set LvlMesh(AllLvlMesh) = New TVMesh
Set LvlMesh(AllLvlMesh) = Scene.CreateMeshBuilder
LvlMesh(AllLvlMesh).Load3DSMesh App.Path + "\Data Files\Models\Tree2.3ds", False, False, False, False, False
LvlMesh(AllLvlMesh).ScaleMesh 5, 5, 5
LvlMesh(AllLvlMesh).SetPosition (I * 100) + 60, 0, (I4 * 100) + 45
LvlMesh(AllLvlMesh).SetTexture GetTex("Tree2")
AllLvlMesh = AllLvlMesh + 1
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
LvlColl(AllLvlColl).X = (I * 100) + 50
LvlColl(AllLvlColl).z = (I4 * 100) + 50
LvlColl(AllLvlColl).y = 7
AllLvlColl = AllLvlColl + 1
GoTo 190
End If
If X0 = "1" Then
TankPosition.X = (I * 100) + 50
TankPosition.z = (I4 * 100) + 50
TankPosition.y = 10
TankPositionF = TankPosition
TankActor.SetPosition TankPosition.X, TankPosition.y, TankPosition.z
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
GoTo 190
End If
If X0 = "2" Then
Tank2Position.X = (I * 100) + 50
Tank2Position.z = (I4 * 100) + 50
Tank2Position.y = 10
Tank2PositionF = Tank2Position
TankActor2.SetPosition Tank2Position.X, Tank2Position.y, Tank2Position.z
Level.AddFloor GetTex(X1), (I * 100), (I4 * 100), (I * 100) + 100, (I4 * 100) + 100, 0
sng2AngleY = 3.2
GoTo 190
End If
190:
Next
Next
Input #1, X0
I4 = Int(X0)
AllDBDestinationP1 = I4
For I5 = 0 To I4 - 1
Input #1, X0
I2 = Int(X0)
AllDBDestination(I5) = I2 - 1
For I = 0 To I2 - 1
Input #1, X0
I3 = Int(X0) - 1
TankDBDestination(I, I5).z = (I3 * 100) + 50
Input #1, X0
I3 = Int(X0) - 1
TankDBDestination(I, I5).X = (I3 * 100) + 50
TankDBDestination(I, I5).y = 15
Next
Next
Input #1, X0
I4 = Int(X0)
AllDBDestinationP2 = I4
For I5 = 0 To I4 - 1
Input #1, X0
I2 = Int(X0)
AllDBDestination2(I5) = I2 - 1
For I = 0 To I2 - 1
Input #1, X0
I3 = Int(X0) - 1
TankDBDestination2(I, I5).z = (I3 * 100) + 50
Input #1, X0
I3 = Int(X0) - 1
TankDBDestination2(I, I5).X = (I3 * 100) + 50
TankDBDestination2(I, I5).y = 15
Next
Next
Close #1
AllTankGActor = AllTankGActor - 1
End Sub

Private Sub Check_Bump_Kill()
For I = 0 To AllTankGActor
TankGActorGP = TankGActorG(I).GetPosition

If TankGActorFC(I) = "W" Then
If TankGActorH(I) > 0 Then
TankGActorH(I) = TankGActorH(I) - 1
GoTo 190
Else
TankGActor(I).SetColor RGBA(1, 1, 1, 1)
TankGActorH(I) = 20
TankGActorFC(I) = "N"
End If
End If

' ÇÕØÏÇã ÇáØáÞÇÊ ÈÇáÌÏÇÑ
For I2 = 0 To AllLvlColl
If Gun1ON = True Then
If GetDistance3D(TankActorGun.GetPosition.X, 0, TankActorGun.GetPosition.z, LvlColl(I2).X, 0, LvlColl(I2).z) < LvlColl(I2).y - 5 Then
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActorGun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankActorGun.SetPosition TankActor.GetPosition.X, TankActor.GetPosition.y, TankActor.GetPosition.z
Gun1ON = False
End If
End If
If Gun2ON = True Then
If GetDistance3D(TankActor2Gun.GetPosition.X, 0, TankActor2Gun.GetPosition.z, LvlColl(I2).X, 0, LvlColl(I2).z) < LvlColl(I2).y - 5 Then
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActor2Gun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankActor2Gun.SetPosition TankActor2.GetPosition.X, TankActor2.GetPosition.y, TankActor2.GetPosition.z
Gun2ON = False
End If
End If
If Not TankGActor(I).GetPosition.y > 45 Then
If GetDistance3D(TankGActorG(I).GetPosition.X, 0, TankGActorG(I).GetPosition.z, LvlColl(I2).X, 0, LvlColl(I2).z) < LvlColl(I2).y - 5 Then
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankGActorG(I).GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
GoTo 20
End If
End If
Next
20:

If TankGActorFC(I) = "2" Then
' ÓíÞÊá ÏÈÇÈÉ ááÇÚÈ ÇáÂÎÑ
For I2 = 0 To 19
If TankDBH1(I2) > 0 Then
If GetDistance3D(TankDB1(I2).GetPosition.X, 0, TankDB1(I2).GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 500 Then
If GetDistance3D(TankGActorG(I).GetPosition.X, 0, TankGActorG(I).GetPosition.z, TankDB1(I2).GetPosition.X, 0, TankDB1(I2).GetPosition.z) > 10 Then
TankGActorGP = VNormalize(VSubtract(TankDB1(I2).GetPosition, TankGActorGP))
TankGActorGP = VAdd(TankGActorG(I).GetPosition, VScale(TankGActorGP, 9))
TankGActorG(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
GoTo 192
Else ' ÇáØáÞÉ ÓÊÞÊá ÇáÏÈÇÈÉ ááÇÚÈ ÇáÂÎÑ
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankGActorG(I).GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
TankDBH1(I2) = TankDBH1(I2) - 1
End If
End If
End If
Next
' ÓíÞÊá ÇááÇÚÈ ÇáÃæá
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 500 Then
' ÇááÇÚÈ Úáì ãÓÇÝÉ ãäÇÓÈÉ
If GetDistance3D(TankGActorG(I).GetPosition.X, 0, TankGActorG(I).GetPosition.z, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) > 10 Then
TankGActorGP = VNormalize(VSubtract(TankActor.GetPosition, TankGActorGP))
TankGActorGP = VAdd(TankGActorG(I).GetPosition, VScale(TankGActorGP, 9))
TankGActorG(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
GoTo 192
Else ' ÇáØáÞÉ ÓÊÞÊá ÇááÇÚÈ ÇáÃæá
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankGActorG(I).GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
TankActorH = TankActorH - 2
Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
End If
End If
' ÓíÞÊá ÇáãÏÇÝÚ ÇáÚÏæÉ ÇáÃÎÑì | Kill Coms Bombs
For I2 = 0 To AllTankGActor
If TankGActorFC(I2) = "1" Then
If GetDistance3D(TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z, TankGActor(I2).GetPosition.X, 0, TankGActor(I2).GetPosition.z) < 500 Then
If GetDistance3D(TankGActorG(I).GetPosition.X, 0, TankGActorG(I).GetPosition.z, TankGActor(I2).GetPosition.X, 0, TankGActor(I2).GetPosition.z) > 10 Then
TankGActorGP = VNormalize(VSubtract(TankGActor(I2).GetPosition, TankGActorGP))
TankGActorGP = VAdd(TankGActorG(I).GetPosition, VScale(TankGActorGP, 9))
TankGActorG(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
Else
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankGActorG(I).GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
TankGActorH(I2) = TankGActorH(I2) - 1
End If
End If
End If
Next

End If

192:
If TankGActorFC(I) = "1" Then
' ÓíÞÊá ÏÈÇÈÉ ááÇÚÈ ÇáÂÎÑ
For I2 = 0 To 19
If TankDBH2(I2) > 0 Then
If GetDistance3D(TankDB2(I2).GetPosition.X, 0, TankDB2(I2).GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 500 Then
If GetDistance3D(TankGActorG(I).GetPosition.X, 0, TankGActorG(I).GetPosition.z, TankDB2(I2).GetPosition.X, 0, TankDB2(I2).GetPosition.z) > 10 Then
TankGActorGP = VNormalize(VSubtract(TankDB2(I2).GetPosition, TankGActorGP))
TankGActorGP = VAdd(TankGActorG(I).GetPosition, VScale(TankGActorGP, 9))
TankGActorG(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
GoTo 191
Else ' ÇáØáÞÉ ÓÊÞÊá ÇáÏÈÇÈÉ ááÇÚÈ ÇáÂÎÑ
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankGActorG(I).GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
TankDBH2(I2) = TankDBH2(I2) - 1
End If
End If
End If
Next
' ÓíÞÊá ÇááÇÚÈ ÇáËÇäí
If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z) < 500 Then
' ÇááÇÚÈ Úáì ãÓÇÝÉ ãäÇÓÈÉ
If GetDistance3D(TankGActorG(I).GetPosition.X, 0, TankGActorG(I).GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) > 10 Then
TankGActorGP = VNormalize(VSubtract(TankActor2.GetPosition, TankGActorGP))
TankGActorGP = VAdd(TankGActorG(I).GetPosition, VScale(TankGActorGP, 9))
TankGActorG(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
Else ' ÇáØáÞÉ ÓÊÞÊá ÇááÇÚÈ ÇáËÇäí
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankGActorG(I).GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
TankActor2H = TankActor2H - 2
Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
End If
End If
' ÓíÞÊá ÇáãÏÇÝÚ ÇáÚÏæÉ ÇáÃÎÑì | Kill Coms Bombs
For I2 = 0 To AllTankGActor
If TankGActorFC(I2) = "2" Then
If GetDistance3D(TankGActor(I).GetPosition.X, 0, TankGActor(I).GetPosition.z, TankGActor(I2).GetPosition.X, 0, TankGActor(I2).GetPosition.z) < 500 Then
If GetDistance3D(TankGActorG(I).GetPosition.X, 0, TankGActorG(I).GetPosition.z, TankGActor(I2).GetPosition.X, 0, TankGActor(I2).GetPosition.z) > 10 Then
TankGActorGP = VNormalize(VSubtract(TankGActor(I2).GetPosition, TankGActorGP))
TankGActorGP = VAdd(TankGActorG(I).GetPosition, VScale(TankGActorGP, 9))
TankGActorG(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
Else
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankGActorG(I).GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
TankGActorH(I2) = TankGActorH(I2) - 1
End If
End If
End If
Next

End If

191:
' åá ÇáãÏÝÚ ÞÏ ÞÊá
If TankGActorH(I) < 1 Then
TankGActorG(I).SetPosition TankGActor(I).GetPosition.X, TankGActor(I).GetPosition.y, TankGActor(I).GetPosition.z
TankGActor(I).SetTexture GetTex("BumbNTexture")
TankGActor(I).SetColor RGBA(0, 0, 0, 0.5)
TankGActorH(I) = 500
TankGActorFC(I) = "W"
If TankGActorFC(I) = "1" Then
P2Money = P2Money + 1
Else
P1Money = P1Money + 1
End If
End If

190:
Next
End Sub

Private Sub Gun_Bombing()
For I = 0 To 10
If BombingOF(I) = False Then
BombingFree = I
End If

If BombingOF(I) = True Then
If BombingScal(I) < BombingFreeBig(I) Then
BombingScal(I) = BombingScal(I) + (BombingFreeBig(I) / 20)
Bombing(I, 0).ScaleMesh BombingScal(I), BombingScal(I), BombingScal(I)
Bombing(I, 0).SetPosition BombingPos(I).X, BombingPos(I).y, BombingPos(I).z
Bombing(I, 0).SetColor RGBA(0.5, 0.4, 0.1, 1 - (BombingScal(I) / (BombingFreeBig(I) + 1)))
Bombing(I, 1).ScaleMesh BombingScal(I) / 3, BombingScal(I) / 2, BombingScal(I) / 3
Bombing(I, 1).SetColor RGBA(0.4, 0.3, 0.1, 1 - (BombingScal(I) / (BombingFreeBig(I) + 1)))
Bombing(I, 1).SetPosition BombingPos(I).X, BombingPos(I).y, BombingPos(I).z
Bombing(I, 2).SetColor RGBA(0.4, 0.3, 0, 1 - (BombingScal(I) / (BombingFreeBig(I) + 1)))
Bombing(I, 2).SetPosition BombingPos(I).X, BombingPos(I).y, BombingPos(I).z
Bombing(I, 2).ScaleMesh BombingScal(I) / 4, BombingScal(I) / 4, BombingScal(I) / 4
Else
Bombing(I, 0).SetPosition -1000, -1000, -1000
Bombing(I, 1).SetPosition -1000, -1000, -1000
Bombing(I, 2).SetPosition -1000, -1000, -1000
BombingOF(I) = False
BombingFreeBig(I) = Rnd * 10
End If
End If

Next
End Sub

Private Sub Check_Move_Gun_Players()
If Gun1ON = True Then
TankGActorGP = Tank1Destination
If P1GnB = False Then
' åá ÇáØáÞÉ ÃÕÇÈÊ ÇáåÏÝ Ãã áÇ
If GetDistance3D(TankActorGun.GetPosition.X, 0, TankActorGun.GetPosition.z, Tank1Destination.X, 0, Tank1Destination.z) > 15 Then
Tank1Direction = VNormalize(VSubtract(Tank1Destination, TankActorGun.GetPosition))
TankGActorGP = VAdd(TankActorGun.GetPosition, VScale(Tank1Direction, 10))
TankActorGun.SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
Else
' ÇáØáÞÉ ãä ÓÊÞÊá ¿ áÇÚÈ Ãã ãÏÝÚ
If Gun1Kill = -1 Then
If GetDistance3D(TankActorGun.GetPosition.X, 0, TankActorGun.GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 32 Then
TankActor2H = TankActor2H - 1
Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
End If
Else
TankGActorH(Gun1Kill) = TankGActorH(Gun1Kill) - 1
End If
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActorGun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankActorGun.SetPosition TankActor.GetPosition.X, TankActor.GetPosition.y, TankActor.GetPosition.z
Gun1ON = False
End If
End If
Else
TankActorGun.SetPosition TankActor.GetPosition.X, TankActor.GetPosition.y, TankActor.GetPosition.z
End If

If Gun1ON = True Then
If P1GnB = True Then
TankGActorGP = Tank1Destination
If P1GnW < 30 Then
P1GnW = P1GnW + 1
TankActorGun.SetPosition TankActor.GetPosition.X, TankActor.GetPosition.y, TankActor.GetPosition.z
TankActorGun.ScaleMesh P1GnW * 0.1, P1GnW * 0.1, P1GnW * 0.1
TankActorGun.SetColor RGBA(1, 0, 0, -(P1GnW / 3) * 0.1)
Else
If Gun1Kill = -1 Then
If GetDistance3D(TankActorGun.GetPosition.X, 0, TankActorGun.GetPosition.z, TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z) < 100 Then
TankActor2H = TankActor2H - 10
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActorGun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
End If
Else
If GetDistance3D(TankActorGun.GetPosition.X, 0, TankActorGun.GetPosition.z, TankGActor(Gun1Kill).GetPosition.X, 0, TankGActor(Gun1Kill).GetPosition.z) < 100 Then
TankGActorH(Gun1Kill) = TankGActorH(Gun1Kill) - 10
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActorGun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
End If
End If
P1GnW = 0
Gun1ON = False
TankActorGun.SetColor RGBA(1, 0, 0, 0)
TankActorGun.ScaleMesh 0.1, 0.1, 0.1
End If
End If
End If

If Gun2ON = True Then
TankGActorGP = Tank2Destination
If P2GnB = False Then
' åá ÇáØáÞÉ ÃÕÇÈÊ ÇáåÏÝ Ãã áÇ
If GetDistance3D(TankActor2Gun.GetPosition.X, 0, TankActor2Gun.GetPosition.z, Tank2Destination.X, 0, Tank2Destination.z) > 15 Then
Tank2Direction = VNormalize(VSubtract(Tank2Destination, TankActor2Gun.GetPosition))
TankGActorGP = VAdd(TankActor2Gun.GetPosition, VScale(Tank2Direction, 10))
TankActor2Gun.SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
Else
' ÇáØáÞÉ ãä ÓÊÞÊá ¿ áÇÚÈ Ãã ãÏÝÚ
If Gun2Kill = -1 Then
If GetDistance3D(TankActor2Gun.GetPosition.X, 0, TankActor2Gun.GetPosition.z, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 32 Then
TankActorH = TankActorH - 1
Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
End If
Else
TankGActorH(Gun2Kill) = TankGActorH(Gun2Kill) - 1
End If
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActor2Gun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
TankActor2Gun.SetPosition TankActor2.GetPosition.X, TankActor2.GetPosition.y, TankActor2.GetPosition.z
Gun2ON = False
End If
End If
Else
TankActor2Gun.SetPosition TankActor2.GetPosition.X, TankActor2.GetPosition.y, TankActor2.GetPosition.z
End If

If Gun2ON = True Then
If P2GnB = True Then
TankGActorGP = Tank2Destination
If P2GnW < 30 Then
P2GnW = P2GnW + 1
TankActor2Gun.SetPosition TankActor2.GetPosition.X, TankActor2.GetPosition.y, TankActor2.GetPosition.z
TankActor2Gun.ScaleMesh P2GnW * 0.1, P2GnW * 0.1, P2GnW * 0.1
TankActor2Gun.SetColor RGBA(0, 0, 1, -(P2GnW / 3) * 0.1)
Else
If Gun2Kill = -1 Then
If GetDistance3D(TankActor2Gun.GetPosition.X, 0, TankActor2Gun.GetPosition.z, TankActor.GetPosition.X, 0, TankActor.GetPosition.z) < 100 Then
TankActorH = TankActorH - 10
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActor2Gun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
End If
Else
If GetDistance3D(TankActor2Gun.GetPosition.X, 0, TankActor2Gun.GetPosition.z, TankGActor(Gun2Kill).GetPosition.X, 0, TankGActor(Gun2Kill).GetPosition.z) < 100 Then
TankGActorH(Gun2Kill) = TankGActorH(Gun2Kill) - 10
' ÇäÝÌÇÑ
BombingPos(BombingFree) = TankActor2Gun.GetPosition
BombingScal(BombingFree) = 0
BombingOF(BombingFree) = True
' Çáì åäÇ ÇÌÑÇÁ ÇáÇäÝÌÇÑ
End If
End If
P2GnW = 0
Gun2ON = False
TankActor2Gun.SetColor RGBA(0, 0, 1, 0)
TankActor2Gun.ScaleMesh 0.1, 0.1, 0.1
End If
End If
End If

' ÝÍÕ åá ÇááÇÚÈÇä ÞÏ ÞÊáÇ
If TankActorH < 1 Then
TankActorH = 100
If P1Money > 10 Then
P1Money = P1Money - 10
Else
P1Money = 0
End If
P2Money = P2Money + 10
TankPosition = TankPositionF
TankActor.SetPosition TankPositionF.X, TankPositionF.y, TankPositionF.z
P1Gn = 100
sngAngleY = 0
Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
End If

If TankActor2H < 1 Then
TankActor2H = 100
If P2Money > 10 Then
P2Money = P2Money - 10
Else
P2Money = 0
End If
P1Money = P1Money + 10
Tank2Position = Tank2PositionF
TankActor2.SetPosition Tank2PositionF.X, Tank2PositionF.y, Tank2PositionF.z
P2Gn = 100
sng2AngleY = 3.2
Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
End If
End Sub

Private Sub Check_Players_Coll_Lvl()
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, MKR.X, 0, MKR.z) < 75 Then
sngWalk = 0
sngStrafe = 0
TankActor.SetPosition Pl1Pos.X, Pl1Pos.y, Pl1Pos.z
TankPosition = Pl1Pos
End If
If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, MKR.X, 0, MKR.z) < 75 Then
sng2Walk = 0
sng2Strafe = 0
TankActor2.SetPosition Pl2Pos.X, Pl2Pos.y, Pl2Pos.z
Tank2Position = Pl2Pos
End If
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, MKR2.X, 0, MKR2.z) < 75 Then
sngWalk = 0
sngStrafe = 0
TankActor.SetPosition Pl1Pos.X, Pl1Pos.y, Pl1Pos.z
TankPosition = Pl1Pos
End If
If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, MKR2.X, 0, MKR2.z) < 75 Then
sng2Walk = 0
sng2Strafe = 0
TankActor2.SetPosition Pl2Pos.X, Pl2Pos.y, Pl2Pos.z
Tank2Position = Pl2Pos
End If

For I = 0 To AllLvlColl
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, LvlColl(I).X, 0, LvlColl(I).z) < LvlColl(I).y Then
sngWalk = 0
sngStrafe = 0
TankActor.SetPosition Pl1Pos.X, Pl1Pos.y, Pl1Pos.z
TankPosition = Pl1Pos
End If
If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, LvlColl(I).X, 0, LvlColl(I).z) < LvlColl(I).y Then
sng2Walk = 0
sng2Strafe = 0
TankActor2.SetPosition Pl2Pos.X, Pl2Pos.y, Pl2Pos.z
Tank2Position = Pl2Pos
End If
Next

For I = 0 To 19
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankDB1(I).GetPosition.X, 0, TankDB1(I).GetPosition.z) < 40 Then
sngWalk = 0
sngStrafe = 0
TankActor.SetPosition Pl1Pos.X, Pl1Pos.y, Pl1Pos.z
TankPosition = Pl1Pos
End If
If GetDistance3D(TankActor.GetPosition.X, 0, TankActor.GetPosition.z, TankDB2(I).GetPosition.X, 0, TankDB2(I).GetPosition.z) < 40 Then
sngWalk = 0
sngStrafe = 0
TankActor.SetPosition Pl1Pos.X, Pl1Pos.y, Pl1Pos.z
TankPosition = Pl1Pos
End If
If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, TankDB1(I).GetPosition.X, 0, TankDB1(I).GetPosition.z) < 40 Then
sng2Walk = 0
sng2Strafe = 0
TankActor2.SetPosition Pl2Pos.X, Pl2Pos.y, Pl2Pos.z
Tank2Position = Pl2Pos
End If
If GetDistance3D(TankActor2.GetPosition.X, 0, TankActor2.GetPosition.z, TankDB2(I).GetPosition.X, 0, TankDB2(I).GetPosition.z) < 40 Then
sng2Walk = 0
sng2Strafe = 0
TankActor2.SetPosition Pl2Pos.X, Pl2Pos.y, Pl2Pos.z
Tank2Position = Pl2Pos
End If
Next
End Sub

Private Sub Check_Dbabat()
If TankDBH1Wait > 0 Then
TankDBH1Wait = TankDBH1Wait - 1
End If
If TankDBH2Wait > 0 Then
TankDBH2Wait = TankDBH2Wait - 1
End If

For I = 0 To 19
If TankDBH1(I) > 0 Then
TankGActorGP = TankDB1Destination(I)

' ÌÚá ÇáãÏÇÝÚ ÇáãÌÇæÑÉ ÕÏíÞÉ
For I2 = 0 To AllTankGActor
If TankGActorFC(I2) = "N" Then
If GetDistance3D(TankDB1(I).GetPosition.X, 0, TankDB1(I).GetPosition.z, TankGActor(I2).GetPosition.X, 0, TankGActor(I2).GetPosition.z) < 200 Then
TankGActorFC(I2) = "1"
TankGActor(I2).SetTexture GetTex("BumbTexture")
TankGActor(I2).SetColor RGBA(1, 1, 1, 1)
TankGActorG(I2).SetColor RGBA(0.1, 0, 0, 1)
TankGActorH(I2) = 20
P1Money = P1Money + 1
Label1.Caption = "HB : " + CStr(TankActorH) + "  -  G : " + CStr(P1Gn) + "  -  M : " + CStr(P1Money) + "  $"
End If
End If
Next

' åá ÇáÏÈÇÈÉ æÕáÊ Çáì ÇáåÏÝ ÇáÍÇáí Ãã áÇ
If GetDistance3D(TankDB1(I).GetPosition.X, 0, TankDB1(I).GetPosition.z, TankDB1Destination(I).X, 0, TankDB1Destination(I).z) > 5 Then
TankDB1Direction(I) = VNormalize(VSubtract(TankDB1Destination(I), TankDB1(I).GetPosition))
TankGActorGP = VAdd(TankDB1(I).GetPosition, VScale(TankDB1Direction(I), 1))
TankDB1(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
TankDB1(I).LookAtPoint TankDB1Destination(I)
TankDB1(I).RotateX -90
TankDB1(I).RotateZ 180
Else
If TankDB1DirectionNM(I) < AllDBDestination(TankDB1DestinationN(I)) Then
TankDB1DirectionNM(I) = TankDB1DirectionNM(I) + 1
TankDB1Destination(I) = TankDBDestination(TankDB1DirectionNM(I), TankDB1DestinationN(I))
Else
PWin = 1
End If
End If

Else ' ÇáÏÈÇÈÉ ÞÏ ÞÊáÊ
TankDB1(I).SetPosition 50, -1500, 50
End If

If TankDBH2(I) > 0 Then
TankGActorGP = TankDB2Destination(I)

' ÌÚá ÇáãÏÇÝÚ ÇáãÌÇæÑÉ ÕÏíÞÉ
For I2 = 0 To AllTankGActor
If TankGActorFC(I2) = "N" Then
If GetDistance3D(TankDB2(I).GetPosition.X, 0, TankDB2(I).GetPosition.z, TankGActor(I2).GetPosition.X, 0, TankGActor(I2).GetPosition.z) < 200 Then
TankGActorFC(I2) = "2"
TankGActor(I2).SetTexture GetTex("BumbTexture")
TankGActor(I2).SetColor RGBA(0, 0, 1, 1)
TankGActorG(I2).SetColor RGBA(0, 0, 0.1, 1)
TankGActorH(I2) = 20
P2Money = P2Money + 1
Label2.Caption = "HB : " + CStr(TankActor2H) + "  -  G : " + CStr(P2Gn) + "  -  M : " + CStr(P2Money) + "  $"
End If
End If
Next

' åá ÇáÏÈÇÈÉ æÕáÊ Çáì ÇáåÏÝ ÇáÍÇáí Ãã áÇ
If GetDistance3D(TankDB2(I).GetPosition.X, 0, TankDB2(I).GetPosition.z, TankDB2Destination(I).X, 0, TankDB2Destination(I).z) > 5 Then
TankDB2Direction(I) = VNormalize(VSubtract(TankDB2Destination(I), TankDB2(I).GetPosition))
TankGActorGP = VAdd(TankDB2(I).GetPosition, VScale(TankDB2Direction(I), 1))
TankDB2(I).SetPosition TankGActorGP.X, TankGActorGP.y, TankGActorGP.z
TankDB2(I).LookAtPoint TankDB2Destination(I)
TankDB2(I).RotateX -90
TankDB2(I).RotateZ 180
Else
If TankDB2DirectionNM(I) < AllDBDestination2(TankDB2DestinationN(I)) Then
TankDB2DirectionNM(I) = TankDB2DirectionNM(I) + 1
TankDB2Destination(I) = TankDBDestination2(TankDB2DirectionNM(I), TankDB2DestinationN(I))
Else
PWin = 2
End If
End If

Else ' ÇáÏÈÇÈÉ ÞÏ ÞÊáÊ
TankDB2(I).SetPosition 50, -1500, 50
End If

Next
End Sub

