object BenchmarkForm: TBenchmarkForm
  Left = 300
  Top = 200
  Width = 700
  Height = 550
  Caption = 'Device Benchmark & Diagnostics'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 676
    Height = 470
    ActivePage = TabBenchmark
    TabOrder = 0
    
    object TabBenchmark: TTabSheet
      Caption = 'Benchmark'
      
      object GroupTestType: TGroupBox
        Left = 8
        Top = 8
        Width = 650
        Height = 120
        Caption = ' Select Test Type '
        TabOrder = 0
        
        object RadioSeqRead: TRadioButton
          Left = 16
          Top = 24
          Width = 200
          Height = 17
          Caption = 'Sequential Read (Large Files)'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        
        object RadioSeqWrite: TRadioButton
          Left = 16
          Top = 48
          Width = 200
          Height = 17
          Caption = 'Sequential Write (Destructive!)'
          TabOrder = 1
        end
        
        object RadioRandRead: TRadioButton
          Left = 320
          Top = 24
          Width = 200
          Height = 17
          Caption = 'Random Read (4KB blocks)'
          TabOrder = 2
        end
        
        object RadioRandWrite: TRadioButton
          Left = 320
          Top = 48
          Width = 200
          Height = 17
          Caption = 'Random Write (Destructive!)'
          TabOrder = 3
        end
        
        object LabelTestSize: TLabel
          Left = 16
          Top = 80
          Width = 80
          Height = 13
          Caption = 'Test Size (MB):'
        end
        
        object EditTestSize: TEdit
          Left = 104
          Top = 76
          Width = 80
          Height = 21
          TabOrder = 4
          Text = '100'
        end
        
        object ButtonRunTest: TButton
          Left = 320
          Top = 74
          Width = 120
          Height = 25
          Caption = 'Run Selected Test'
          TabOrder = 5
        end
        
        object ButtonRunAll: TButton
          Left = 450
          Top = 74
          Width = 120
          Height = 25
          Caption = 'Run All Tests'
          TabOrder = 6
        end
      end
      
      object GroupProgress: TGroupBox
        Left = 8
        Top = 136
        Width = 650
        Height = 100
        Caption = ' Progress '
        TabOrder = 1
        
        object LabelCurrentTest: TLabel
          Left = 16
          Top = 24
          Width = 80
          Height = 13
          Caption = 'Current Test:'
        end
        
        object LabelCurrentTestValue: TLabel
          Left = 110
          Top = 24
          Width = 50
          Height = 13
          Caption = 'Ready'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        
        object LabelCurrentSpeed: TLabel
          Left = 16
          Top = 48
          Width = 80
          Height = 13
          Caption = 'Current Speed:'
        end
        
        object LabelCurrentSpeedValue: TLabel
          Left = 110
          Top = 48
          Width = 50
          Height = 13
          Caption = '0.00 MB/s'
        end
        
        object ProgressBar: TProgressBar
          Left = 16
          Top = 70
          Width = 618
          Height = 20
          TabOrder = 0
        end
      end
      
      object GroupResults: TGroupBox
        Left = 8
        Top = 244
        Width = 650
        Height = 180
        Caption = ' Results '
        TabOrder = 2
        
        object MemoResults: TMemo
          Left = 8
          Top = 20
          Width = 634
          Height = 152
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    
    object TabSMART: TTabSheet
      Caption = 'SMART Data'
      
      object GroupSMARTInfo: TGroupBox
        Left = 8
        Top = 8
        Width = 650
        Height = 100
        Caption = ' Device Health '
        TabOrder = 0
        
        object LabelHealthStatus: TLabel
          Left = 16
          Top = 24
          Width = 100
          Height = 13
          Caption = 'Overall Health:'
        end
        
        object LabelHealthValue: TLabel
          Left = 130
          Top = 24
          Width = 60
          Height = 13
          Caption = 'Unknown'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        
        object LabelTemperature: TLabel
          Left = 16
          Top = 48
          Width = 100
          Height = 13
          Caption = 'Temperature:'
        end
        
        object LabelTempValue: TLabel
          Left = 130
          Top = 48
          Width = 50
          Height = 13
          Caption = 'N/A'
        end
        
        object LabelPowerOn: TLabel
          Left = 16
          Top = 72
          Width = 100
          Height = 13
          Caption = 'Power-On Hours:'
        end
        
        object LabelPowerOnValue: TLabel
          Left = 130
          Top = 72
          Width = 50
          Height = 13
          Caption = 'N/A'
        end
        
        object ButtonRefreshSMART: TButton
          Left = 520
          Top = 20
          Width = 110
          Height = 25
          Caption = 'Refresh Data'
          TabOrder = 0
        end
        
        object ButtonExportSMART: TButton
          Left = 520
          Top = 55
          Width = 110
          Height = 25
          Caption = 'Export to TXT'
          TabOrder = 1
        end
      end
      
      object GroupSMARTAttributes: TGroupBox
        Left = 8
        Top = 116
        Width = 650
        Height = 308
        Caption = ' SMART Attributes '
        TabOrder = 1
        
        object ListView: TListView
          Left = 8
          Top = 20
          Width = 634
          Height = 280
          Columns = <
            item
              Caption = 'ID'
              Width = 40
            end
            item
              Caption = 'Attribute Name'
              Width = 200
            end
            item
              Caption = 'Value'
              Width = 60
            end
            item
              Caption = 'Worst'
              Width = 60
            end
            item
              Caption = 'Threshold'
              Width = 70
            end
            item
              Caption = 'Raw Value'
              Width = 100
            end
            item
              Caption = 'Status'
              Width = 80
            end>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
    end
    
    object TabDeviceInfo: TTabSheet
      Caption = 'Device Info'
      
      object GroupDeviceDetails: TGroupBox
        Left = 8
        Top = 8
        Width = 650
        Height = 416
        Caption = ' Device Details '
        TabOrder = 0
        
        object MemoDeviceInfo: TMemo
          Left = 8
          Top = 20
          Width = 634
          Height = 388
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  
  object ButtonClose: TButton
    Left = 594
    Top = 486
    Width = 90
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    ModalResult = 1
  end
  
  object ButtonCancel: TButton
    Left = 488
    Top = 486
    Width = 90
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
  end
end
