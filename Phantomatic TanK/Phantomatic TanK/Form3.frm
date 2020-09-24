VERSION 5.00
Begin VB.Form Form3 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Zaid"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   12000
   Icon            =   "Form3.frx":0000
   LinkTopic       =   "Form3"
   ScaleHeight     =   9000
   ScaleWidth      =   12000
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.FileListBox File1 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0FFC0&
      Height          =   750
      Left            =   120
      Pattern         =   "*.LVL*"
      TabIndex        =   17
      Top             =   7600
      Width           =   5775
   End
   Begin VB.TextBox Text2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H008080FF&
      Height          =   2535
      Left            =   6120
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   15
      Text            =   "Form3.frx":1CCA
      Top             =   4800
      Width           =   5775
   End
   Begin VB.TextBox Text1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF8080&
      Height          =   2535
      Left            =   120
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   14
      Text            =   "Form3.frx":1E79
      Top             =   4800
      Width           =   5775
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H00008000&
      ForeColor       =   &H80000008&
      Height          =   3255
      Left            =   0
      ScaleHeight     =   3225
      ScaleWidth      =   11985
      TabIndex        =   5
      Top             =   1440
      Visible         =   0   'False
      Width           =   12015
      Begin VB.Timer Timer1 
         Enabled         =   0   'False
         Interval        =   250
         Left            =   0
         Top             =   0
      End
      Begin VB.Label Label31 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BackStyle       =   0  'Transparent
         Caption         =   "Win The Game \ ÝæÒ ÇááÚÈÉ"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   13.5
            Charset         =   178
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   375
         Left            =   7560
         TabIndex        =   13
         Top             =   2280
         Width           =   4335
      End
      Begin VB.Label Label29 
         Caption         =   "Label27"
         Height          =   255
         Left            =   960
         TabIndex        =   12
         Top             =   0
         Visible         =   0   'False
         Width           =   375
      End
      Begin VB.Line Line4 
         BorderColor     =   &H00004000&
         X1              =   3960
         X2              =   7320
         Y1              =   1440
         Y2              =   1440
      End
      Begin VB.Label Label30 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   13.5
            Charset         =   178
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   1095
         Left            =   3960
         TabIndex        =   11
         Top             =   1680
         Width           =   3375
      End
      Begin VB.Line Line3 
         BorderColor     =   &H00C0FFC0&
         X1              =   7440
         X2              =   7440
         Y1              =   0
         Y2              =   3240
      End
      Begin VB.Label Label28 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   13.5
            Charset         =   178
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   975
         Left            =   3960
         RightToLeft     =   -1  'True
         TabIndex        =   10
         Top             =   360
         Width           =   3375
      End
      Begin VB.Label Label27 
         Caption         =   "Label27"
         Height          =   255
         Left            =   480
         TabIndex        =   9
         Top             =   0
         Visible         =   0   'False
         Width           =   375
      End
      Begin VB.Label Label26 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BackStyle       =   0  'Transparent
         Caption         =   "Gaining Friends \ ÇßÊÓÇÈ ÕÏíÞ"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   13.5
            Charset         =   178
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   375
         Left            =   7560
         TabIndex        =   8
         Top             =   1560
         Width           =   4335
      End
      Begin VB.Label Label25 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BackStyle       =   0  'Transparent
         Caption         =   "Acciting Tank \ ÇÓÊÏÚÇÁ ÏÈÇÈÉ"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   13.5
            Charset         =   178
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   375
         Left            =   7560
         TabIndex        =   7
         Top             =   840
         Width           =   4335
      End
      Begin VB.Label Label24 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BackStyle       =   0  'Transparent
         Caption         =   "Informations About \ ãÚáæãÇÊ Íæá"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   13.5
            Charset         =   178
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0080FF80&
         Height          =   375
         Left            =   7560
         TabIndex        =   6
         Top             =   120
         Width           =   4335
      End
      Begin VB.Image Image2 
         Height          =   2415
         Left            =   360
         Stretch         =   -1  'True
         Top             =   360
         Width           =   3495
      End
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Select Level \ ÇÎÊíÇÑ ÇáãÓÊæì"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0FFC0&
      Height          =   375
      Left            =   6840
      TabIndex        =   16
      Top             =   7800
      Width           =   4335
   End
   Begin VB.Line Line7 
      BorderColor     =   &H00FFFFFF&
      X1              =   0
      X2              =   12000
      Y1              =   7440
      Y2              =   7440
   End
   Begin VB.Image Image3 
      Height          =   735
      Left            =   11280
      Top             =   0
      Width           =   735
   End
   Begin VB.Line Line6 
      BorderColor     =   &H00FFFFFF&
      BorderWidth     =   5
      X1              =   11400
      X2              =   11880
      Y1              =   120
      Y2              =   600
   End
   Begin VB.Line Line5 
      BorderColor     =   &H00FFFFFF&
      BorderWidth     =   5
      X1              =   11880
      X2              =   11400
      Y1              =   120
      Y2              =   600
   End
   Begin VB.Shape Shape1 
      BorderColor     =   &H00FFFFFF&
      Height          =   735
      Left            =   11280
      Top             =   0
      Width           =   735
   End
   Begin VB.Label Label23 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Start Game \ ÈÏÁ ÇááÚÈÉ"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   495
      Left            =   6000
      TabIndex        =   4
      Top             =   8570
      Width           =   6015
   End
   Begin VB.Label Label22 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Informations \ ãÚáæãÇÊ"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   495
      Left            =   0
      TabIndex        =   3
      Top             =   8570
      Width           =   6015
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      X1              =   0
      X2              =   12000
      Y1              =   8520
      Y2              =   8520
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00FFFFFF&
      X1              =   6000
      X2              =   6000
      Y1              =   4680
      Y2              =   7440
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Players's Control \ ÊÍßã ÇááÇÚÈíä"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   615
      Left            =   0
      TabIndex        =   2
      Top             =   1560
      Width           =   12015
   End
   Begin VB.Image Image1 
      Height          =   2235
      Left            =   1320
      Picture         =   "Form3.frx":200D
      Top             =   2400
      Width           =   9255
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "ÇáÞÇÆãÉ ÇáÑÆíÓíÉ ááÏÈÇÈÉ ÇáÔÈÍíÉ"
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
      TabIndex        =   1
      Top             =   840
      Width           =   12015
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Phantomatic TanK Main Menu"
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
      TabIndex        =   0
      Top             =   240
      Width           =   12015
   End
End
Attribute VB_Name = "Form3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
On Error GoTo 5
File1.Path = App.Path + "\Data Files\Missions\"
File1.Refresh
File1.ListIndex = 0
Exit Sub
5:
End
End Sub

Private Sub Image3_Click()
Unload Me
End Sub

Private Sub Label22_Click()
If Picture1.Visible = False Then
Picture1.Visible = True
Else
Picture1.Visible = True
End If
End Sub

Private Sub Label23_Click()
Form8.Show
Form8.Timer1.Tag = Left(File1.List(File1.ListIndex), Len(File1.List(File1.ListIndex)) - 4)
Unload Me
End Sub

Private Sub Label25_Click()
Label27.Caption = "0"
Label29.Caption = "CreateTank"
Timer1.Interval = 250
Timer1.Enabled = True
End Sub

Private Sub Label26_Click()
Label27.Caption = "0"
Label29.Caption = "CreateBomb"
Timer1.Interval = 250
Timer1.Enabled = True
End Sub

Private Sub Label31_Click()
Label27.Caption = "0"
Label29.Caption = "WinGame"
Timer1.Interval = 250
Timer1.Enabled = True
End Sub

Private Sub Timer1_Timer()
On Error GoTo 5
Timer1.Interval = 100
Label27.Caption = Format(Int(Label27.Caption) + 1)
Image2.Picture = LoadPicture("C:\ZaNaZeeN\" + Label29.Caption + "\" + Label27.Caption + ".jpg")

Load_text

Exit Sub
5:
Timer1.Enabled = False
Label28.Caption = ""
Label30.Caption = ""
End Sub

Private Sub Load_text()
On Error GoTo 6
Dim X As String
Open "C:\ZaNaZeeN\" + Label29.Caption + "\" + Label27.Caption + ".Txt" For Input As #1
Input #1, X
Label28.Caption = X
Input #1, X
Label30.Caption = X
Close #1
Timer1.Interval = 2000
Exit Sub
6:
Close #1
End Sub
