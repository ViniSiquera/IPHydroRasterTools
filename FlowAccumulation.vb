'*******************************************************************
'DETERMINAÇÃO DE ÁREA ACUMULADA
'Criado por Vinícius Alencar Siqueira - 20/01/2014
'Cálculo do número de células acumuladas baseado em Haverkort e Janssen (2012) - Simple I/O efficient flow accumulation on grid terrains
'*******************************************************************
Imports IPHDataManagement

''' <summary>
''' Classe que calcula o número de células acumuladas a partir de um raster com direções de fluxo
''' </summary>
Public Class FlowAccumulation

    Private _FlowDirection As RasterInteger
    Private _FlowAcc As RasterReal
    Private _CheckedNodeList(,) As Boolean

#Region "Métodos e funções"
    ''' <summary>
    ''' Retorna o MDE original ou modificado pelo processo
    ''' </summary>
    Public Function GetFlowAccumulationMatrix() As RasterReal
        Return _FlowAcc.CopyData
    End Function

    ''' <summary>
    ''' Retorna o MDE original ou modificado pelo processo
    ''' </summary>
    Public Function GetFlowDirectionMatrix() As RasterInteger
        Return _FlowDirection.CopyData
    End Function
    Public Sub SetFlowDirectionData(ByVal FD As RasterInteger)
        _FlowDirection = FD
        _FlowAcc = New RasterReal(_FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.XllCorner, _FlowDirection.YllCorner, _FlowDirection.Cellsize, _FlowDirection.NoDataValue)
    End Sub
#End Region

    'Cria um novo arquivo para
    Public Sub New()
        Me._FlowDirection = New RasterInteger()
    End Sub

    'Lê um arquivo de MDE, especificando o local
    Public Sub ReadFlowDirection(ByVal Local As String)

        Me._FlowDirection.ReadData(Local)
        _FlowAcc = New RasterReal(_FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.XllCorner, _FlowDirection.YllCorner, _FlowDirection.Cellsize, _FlowDirection.NoDataValue)

    End Sub

    ''' <summary>
    ''' 'Calcula o número de células acumuladas de acordo com o Flow Direction selecionado
    ''' </summary>
    Public Sub Runoff()

        Dim posX, posY As Int16
        Dim totalCells As Long

        ReDim _CheckedNodeList(_FlowDirection.Linhas - 1, _FlowDirection.Colunas - 1)

        For i = 0 To _FlowDirection.Linhas - 1
            For j = 0 To _FlowDirection.Colunas - 1

                posX = j
                posY = i

                If _CheckedNodeList(posY, posX) = False Then 'Se a célula ainda não foi checada
                    Do
                        If NeighbourCellsAnalyzed(posY, posX) = True Then 'Verifica se todas as células vizinhas já foram analisadas; Caso sim:
                            totalCells = CLng(_FlowAcc.Dados(posY, posX)) 'Armazena o número de células acumuladas
                            _CheckedNodeList(posY, posX) = True 'Indica que a célula foi analisada
                            FlowDirection.MoveToFlowDirection(_FlowDirection.Dados(posY, posX), posY, posX) 'Move na direção do flow direction

                            If posX < 0 OrElse posY < 0 OrElse posX > (_FlowDirection.Colunas - 1) OrElse posY > (_FlowDirection.Linhas - 1) Then Exit Do 'Se extrapolar os limites da matriz, sai do loop
                            If _FlowDirection.Dados(posY, posX) = _FlowDirection.NoDataValue Then _FlowAcc.Dados(posY, posX) = _FlowDirection.NoDataValue : Exit Do 'Se chegar em um NODATA, sai do loop

                            _FlowAcc.Dados(posY, posX) += (totalCells + 1) 'Acumula as células anteriores
                        Else
                            Exit Do
                        End If
                    Loop
                    totalCells = 0 'Reinicia para a próxima célula
                End If
            Next
        Next

    End Sub

    'Verifica se algum vizinho de mesma cota, ou inferior já possui flow direction, atribuindo a mesma em caso verdadeiro
    Private Function NeighbourCellsAnalyzed(ByVal Yc As Int16, ByVal xc As Int16) As Boolean

        'na openlist:,         'direções de apontamento para a célula central
        'For y = -1 To 1       135  180  225       
        'posY = (Yc + y)       90    x   270       
        'For x = -1 To 1       45   360  315       
        'posX = (xc + x)

        Dim xi, yi As Int16
        Dim count As Integer = 0
        Dim CellContribution As Boolean = False

        For y = -1 To 1
            yi = (Yc + y)
            For x = -1 To 1
                xi = (xc + x)

                If x <> 0 OrElse y <> 0 Then 'Evita a análise da própria célula (nó central)
                    If xi >= 0 AndAlso yi >= 0 AndAlso xi < _FlowDirection.Colunas AndAlso yi < _FlowDirection.Linhas Then 'Evita sair dos limites do raster

                        If FlowDirection.RelativeIncipientFlowDirection(yi, Yc, xi, xc) = _FlowDirection.Dados(yi, xi) Then CellContribution = True 'Se houver uma das células vizinhas apontando para o centro, aciona o flag

                        If CellContribution = True Then 'Se a célula vizinha estiver apontando para o centro, mas não tiver sido checada ainda, retorna falso.
                            If _CheckedNodeList(yi, xi) = False Then Return False
                        End If

                        CellContribution = False 'Reseta para verificar a próxima célula

                    End If
                    count += 1
                End If
            Next
        Next

        'Se passar por todas as células, e nenhuma contribuir ou todas já tiverem sido checadas então
        Return True

    End Function

    'Escreve os dados de Flow Direction
    Public Sub WriteFlowAccData(ByVal arquivo As String)
        _FlowAcc.WriteData(arquivo)
    End Sub

End Class
