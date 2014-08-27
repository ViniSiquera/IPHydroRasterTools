'*******************************************************************
'DETERMINAÇÃO DO STREAM SEGMENTATION
'Criado por Vinícius Alencar Siqueira - 20/01/2014 jiuihuhh
'*******************************************************************
Imports IPHDataManagement

''' <summary>
''' Classe que define os trechos de rio
''' </summary>
Public Class StreamSegmentation

    Private _StrDef As RasterInteger
    Private _StrSeg As RasterInteger
    Private _FlowDirection As RasterInteger

    Public Sub New()
        _FlowDirection = New RasterInteger
    End Sub

    'Lê os dados do FlowDirection
    Public Sub ReadFlowDirectionData(ByVal local As String)
        _FlowDirection = New RasterInteger
        _FlowDirection.ReadData(local)
        _StrSeg = New RasterInteger(_FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.XllCorner, _FlowDirection.YllCorner, _FlowDirection.Cellsize, _FlowDirection.NoDataValue)
    End Sub

    'Lê os dados do FlowDirection
    Public Sub ReadStreamDefinitionData(ByVal local As String)
        _StrDef = New RasterInteger
        _StrDef.ReadData(local)
    End Sub

    Public Sub SetFlowDirectionData(ByVal FD As RasterInteger)
        _FlowDirection = FD
        _StrSeg = New RasterInteger(_FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.XllCorner, _FlowDirection.YllCorner, _FlowDirection.Cellsize, _FlowDirection.NoDataValue)
    End Sub
    ''' <summary>
    ''' Separa os trechos de rio em todas as confluências
    ''' </summary>
    Public Sub SegmentStreams()

        Dim SegmentNumber As Integer = 1
        Dim xInicial, xFinal, yInicial, yFinal, i, j As Integer
        Dim JunctionFound As Boolean = False

        For row = 0 To _StrSeg.Linhas - 1
            For col = 0 To _StrSeg.Colunas - 1
                i = row
                j = col

                If _StrDef.Dados(row, col) = _StrDef.NoDataValue Then 'Atribui valores de NODATA 
                    _StrSeg.Dados(row, col) = _StrDef.NoDataValue

                ElseIf _StrDef.Dados(row, col) = 1 Then 'Caso estiver sobre a rede de drenagem
                    If _StrSeg.Dados(row, col) = 0 Then 'Caso a célula estiver sobre um trecho ainda não segmentado

                        xInicial = j : yInicial = i 'Identifica a linha e a coluna iniciais

                        Do
                            xFinal = j : yFinal = i
                            FlowDirection.MoveToFlowDirection(_FlowDirection.Dados(i, j), i, j) 'Move em direção à próxima célula

                            If i < 0 OrElse j < 0 OrElse i > _StrDef.Linhas - 1 OrElse j > _StrDef.Colunas - 1 Then 'Se alcançar os limites da matriz, utiliza o atributo da última célula 
                                If _StrSeg.Dados(yFinal, xFinal) = 0 Then
                                    _StrSeg.Dados(yFinal, xFinal) = SegmentNumber
                                    SegmentNumber += 1
                                End If
                                AtualizaSegmentos(yInicial, xInicial, yFinal, xFinal, _StrSeg.Dados(yFinal, xFinal))
                                Exit Do
                            End If

                            If _FlowDirection.Dados(i, j) = _FlowDirection.NoDataValue Then
                                If _StrSeg.Dados(yFinal, xFinal) = 0 Then
                                    _StrSeg.Dados(yFinal, xFinal) = SegmentNumber
                                    SegmentNumber += 1
                                End If
                                AtualizaSegmentos(yInicial, xInicial, yFinal, xFinal, _StrSeg.Dados(yFinal, xFinal))
                                Exit Do 'Passa se mover para um NODATA
                            End If

                            JunctionFound = VerifyStreamOutlet(i, j)
                            If JunctionFound = True Then
                                AtualizaSegmentos(yInicial, xInicial, yFinal, xFinal, SegmentNumber)
                                SegmentNumber += 1 'Aumenta o número dos segmentos identificados
                                Exit Do
                            End If

                            If _StrSeg.Dados(i, j) > 0 Then 'Se chegar em uma parte do trecho com atributo definido, atualiza o restante do trecho atual
                                AtualizaSegmentos(yInicial, xInicial, yFinal, xFinal, _StrSeg.Dados(i, j))
                                Exit Do
                            End If

                        Loop

                        JunctionFound = False

                    End If
                End If
            Next
        Next
    End Sub

    'Verifica se algum vizinho de mesma cota, ou inferior já possui flow direction, atribuindo a mesma em caso verdadeiro
    Private Function VerifyStreamOutlet(ByVal Yc As Int16, ByVal xc As Int16) As Boolean

        'direções de apontamento para a célula central
        'For y = -1 To 1       135  180  225       
        'posY = (Yc + y)       90    x   270       
        'For x = -1 To 1       45   360  315       
        'posX = (xc + x)

        Dim xi, yi As Int16
        Dim nPointingCells As Int16 = 0

        For y = -1 To 1
            yi = (Yc + y)
            For x = -1 To 1
                xi = (xc + x)

                If x <> 0 OrElse y <> 0 Then 'Evita a análise da própria célula (nó central)
                    If xi >= 0 AndAlso yi >= 0 AndAlso xi < _FlowDirection.Colunas AndAlso yi < _FlowDirection.Linhas Then 'Evita sair dos limites do raster

                        If _StrDef.Dados(yi, xi) = 1 Then 'Somente se a célula faz parte da rede de drenagem principal
                            If FlowDirection.RelativeIncipientFlowDirection(yi, Yc, xi, xc) = _FlowDirection.Dados(yi, xi) Then nPointingCells += 1 'Se a célula analisada apontar para a célula de origem, é contabilizada
                        End If
                    End If

                End If
            Next
        Next

        If nPointingCells > 1 Then 'Se a mais de uma célula apontar para a célula central, significa que encontrou uma confluência
            Return True
        Else
            Return False
        End If
    End Function

    'Atualiza o atributo do stream definition
    Private Sub AtualizaSegmentos(ByVal linha As Integer, ByVal coluna As Integer, ByVal LinhaFinal As Integer, ByVal ColunaFinal As Integer, ByVal Atributo As Int16)

        '0 = célula ainda não identificada

        Dim lin As Integer = linha
        Dim col As Integer = coluna

        _StrSeg.Dados(LinhaFinal, ColunaFinal) = Atributo

        Do Until lin = LinhaFinal AndAlso col = ColunaFinal

            _StrSeg.Dados(lin, col) = Atributo

            FlowDirection.MoveToFlowDirection(_FlowDirection.Dados(lin, col), lin, col)
            If col < 0 OrElse lin < 0 OrElse col > (_FlowDirection.Colunas - 1) OrElse lin > (_FlowDirection.Linhas - 1) Then Exit Do 'Cheguei na borda do MDE
        Loop

    End Sub

    'Escreve os dados da matrix dos trechos segmentados
    Public Sub WriteStreamSegmentData(ByVal arquivo As String)
        _StrSeg.WriteData(arquivo)
    End Sub

End Class
