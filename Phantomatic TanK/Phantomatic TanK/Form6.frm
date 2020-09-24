VERSION 5.00
Begin VB.Form Form6 
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   0  'None
   Caption         =   "Form6"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   12000
   Icon            =   "Form6.frx":0000
   LinkTopic       =   "Form6"
   ScaleHeight     =   9000
   ScaleWidth      =   12000
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer4 
      Interval        =   1
      Left            =   720
      Top             =   120
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   2040
      Top             =   4320
   End
   Begin VB.Label Label6 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "ÝíÌæÇá              ÈíÒß"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   615
      Left            =   5160
      TabIndex        =   5
      Top             =   2640
      Width           =   6615
   End
   Begin VB.Image Image5 
      Height          =   750
      Left            =   120
      Picture         =   "Form6.frx":1CCA
      Top             =   4920
      Width           =   9630
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "www.bramjak.com"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00808000&
      Height          =   615
      Left            =   5160
      TabIndex        =   4
      Top             =   4200
      Width           =   6615
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "ááãÒíÏ ãä ÇáÃáÚÇÈ æÇáÈÑãÌíÇÊ ÇáÚÑÈíÉ"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00808000&
      Height          =   615
      Left            =   5160
      TabIndex        =   3
      Top             =   3600
      Width           =   6615
   End
   Begin VB.Image Image4 
      Height          =   990
      Left            =   7680
      Picture         =   "Form6.frx":4A50
      Top             =   2400
      Width           =   1305
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Êã ÈÑãÌÉ ÇááÚÈÉ Úáì áÛÉ ÇáÈÑãÌÉ"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000080&
      Height          =   615
      Left            =   5160
      TabIndex        =   2
      Top             =   1800
      Width           =   6615
   End
   Begin VB.Image Image3 
      Height          =   3300
      Left            =   120
      Picture         =   "Form6.frx":8EA2
      Top             =   5520
      Width           =   4770
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "ÇáÏÈÇÈÉ ÇáÔÈÍíÉ"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00004000&
      Height          =   495
      Left            =   6000
      TabIndex        =   1
      Top             =   960
      Width           =   4335
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Phantomatic TanK"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00004000&
      Height          =   495
      Left            =   6000
      TabIndex        =   0
      Top             =   360
      Width           =   4335
   End
   Begin VB.Image Image1 
      Height          =   4755
      Left            =   240
      Picture         =   "Form6.frx":3C474
      Top             =   240
      Width           =   4800
   End
   Begin VB.Image Image2 
      Height          =   4005
      Left            =   7920
      Picture         =   "Form6.frx":40961
      Top             =   4800
      Width           =   3945
   End
End
Attribute VB_Name = "Form6"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim I As Integer

Private Sub Image5_Click()
Form4.Show
Unload Me
End Sub

Private Sub Timer1_Timer()
On Error GoTo 5
Image1.Picture = LoadPicture("C:\ZaNaZeeN\MG\" + Format(I) + ".Jpg")
I = I + 1
Exit Sub
5:
I = 0
End Sub
