'*******************************************************************
'SEGMENTAÇÃO DE TRECHOS POR COMPRIMENTO MÍNIMO
'Criado por Vinícius Alencar Siqueira - 10/05/2015
'*******************************************************************
Imports IPHDataManagement

''' <summary>
''' Classe que define os trechos de rio
''' </summary>

Public Class LengthSegmentation

    Private _FlowDirection As RasterInteger
    Private _FlowAcc As RasterReal
    Private _StrSeg As RasterInteger
    Private _JunctionCells() As JunctionCell
    Private _JunctionCellsAreas() As Single
    Private _CellAnalyzed(,) As Boolean
    Private _NumberOfJunctionCells As Long
    Private _minLength As Single
    Private _xOutlet, _yOutlet As Int16
    Private _GuessJunctionNumber As Long = 10000 'Número de junções como chute inicial

    Public Sub New(ByVal minLenght As Single)
        _FlowDirection = New RasterInteger
        _minLength = minLenght
    End Sub

    'Lê os dados do FlowDirection
    Public Sub ReadFlowDirectionData(ByVal local As String)
        _FlowDirection = New RasterInteger
        _FlowDirection.ReadData(local)
    End Sub

    'Lê os dados do FlowAccumulation
    Public Sub ReadFlowAccumulationData(ByVal local As String)
        _FlowAcc = New RasterReal
        _FlowAcc.ReadData(local)
    End Sub

    'Lê os dados do StreamDefinition e guarda no segmentation
    Public Sub ReadStreamDefinitionData(ByVal local As String)
        _StrSeg = New RasterInteger
        _StrSeg.ReadData(local)
    End Sub

    ''' <summary>
    ''' Identifica as Junções e marcas os possíveis exutórios
    ''' </summary>
    Public Sub IdentifyJunctionsAndOutlet()

        Dim maxAccum As Single = 0
        Dim i, j As Int16
        Dim xAnterior, yAnterior As Int16
        Dim JunctionFound As Boolean = False
        ReDim _CellAnalyzed(_StrSeg.Linhas - 1, _StrSeg.Colunas - 1) 'Redimensiona a matriz de células analisadas
        ReDim _JunctionCells(_GuessJunctionNumber)
        ReDim _JunctionCellsAreas(_GuessJunctionNumber)
        _NumberOfJunctionCells = -1

        For row = 0 To _StrSeg.Linhas - 1
            For col = 0 To _StrSeg.Colunas - 1
                i = row : yAnterior = row
                j = col : xAnterior = col

                If _FlowAcc.Dados(i, j) > maxAccum Then 'Identificação da célula com exutório
                    _yOutlet = i
                    _xOutlet = j
                    maxAccum = _FlowAcc.Dados(i, j)
                End If

                If _StrSeg.Dados(i, j) > 0 Then 'Caso estiver sobre a drenagem
                    IdentifyAndMarkJunctions(i, j) 'Identifica possíveis junções e marca os exutórios
                    '_CellAnalyzed(i, j) = True 'Marca a célula como sendo analisada

                    Do
                        FlowDirection.MoveToFlowDirection(_FlowDirection.Dados(i, j), i, j) 'Move em direção à próxima célula

                        If i < 0 OrElse j < 0 OrElse i > _StrSeg.Linhas - 1 OrElse j > _StrSeg.Colunas - 1 Then 'Se alcançar os limites da matriz, Não calcula nada e cai fora do loop
                            Exit Do
                        End If

                        If _FlowAcc.Dados(i, j) > maxAccum Then 'Identificação da célula com exutório
                            _yOutlet = i
                            _xOutlet = j
                            maxAccum = _FlowAcc.Dados(i, j)
                        End If

                        IdentifyAndMarkJunctions(i, j) 'Identifica possíveis junções e marca os exutórios
                        _CellAnalyzed(yAnterior, xAnterior) = True '

                        If _CellAnalyzed(i, j) = True Then Exit Do 'Se a célula já foi analisada, sai do loop
                        '_CellAnalyzed(i, j) = True 'Marca a célula como sendo analisada

                        yAnterior = i
                        xAnterior = j

                        If _FlowDirection.Dados(i, j) = _FlowDirection.NoDataValue Then
                            'Achou o exutório da bacia
                            Exit Do 'Passa se mover para um NODATA
                        End If
                    Loop
                End If

            Next
        Next

        _NumberOfJunctionCells += 1 'Para acrescentar o exutório da bacia

        ReDim Preserve _JunctionCells(_NumberOfJunctionCells) 'Deixa o vetor de células de junção com a dimensão correta
        ReDim Preserve _JunctionCellsAreas(_NumberOfJunctionCells) 'Deixa o vetor de áreas com a dimensão correta

        'Acrescenta o exutório da bacia
        _JunctionCells(_NumberOfJunctionCells) = New JunctionCell(_yOutlet, _xOutlet)
        _JunctionCellsAreas(_NumberOfJunctionCells) = maxAccum

        Array.Sort(_JunctionCellsAreas, _JunctionCells) 'Reordena as matriz de acordo com a área dos exutórios

    End Sub

    ''' <summary>
    ''' Segmenta a rede por um comprimento máximo, não levando em consideração as junções da rede.
    ''' </summary>
    Public Sub SegmentBySpecificLength()

        ReDim _CellAnalyzed(_StrSeg.Linhas, _StrSeg.Colunas) 'Reinicializa o vetor das células analizadas
        Dim SegmentNumber As Integer = 1
        Dim xAnterior, yAnterior As Int16
        Dim row, col As Int16
        Dim yLat, xLon As Single
        Dim totalLength As Single = 0

        For nJunction = _NumberOfJunctionCells To 0 Step -1 'Faz da maior área para a menor

            totalLength = 0
            col = _JunctionCells(nJunction).x
            row = _JunctionCells(nJunction).y
            If _CellAnalyzed(row, col) = True Then GoTo nextjunction 'Se a célula já foi analisada, passa para a próxima junção

            Do 'Faça até chegar na cabeceira do rio mais longo
                xAnterior = col
                yAnterior = row

                _CellAnalyzed(row, col) = True
                _StrSeg.Dados(row, col) = SegmentNumber 'enumera o trecho segmentado

                MoveToUpstream(yAnterior, xAnterior, row, col, False) 'Move em direção à célula à montante com maior área

                If row = yAnterior AndAlso col = xAnterior Then 'Significa que chegou na célula de cabeceira

                    Do While totalLength < _minLength 'Faça até atingir o limite de extensão
                        MoveToUpstream(yAnterior, xAnterior, row, col, True) 'Move em direção à célula à montante com maior área, quebrando a barreira do stream definition

                        If row = yAnterior AndAlso col = xAnterior Then Exit Do 'Significa que chegou na célula mais remota da bacia, vai para a próxima junção

                        _CellAnalyzed(row, col) = True 'Marca a célula como analisada
                        _StrSeg.Dados(row, col) = _StrSeg.Dados(yAnterior, xAnterior) 'enumera o trecho segmentado de acordo com o trecho anterior

                        totalLength += (IPHDataManagement.SpatialAnalyst.CellLength(yLat, xLon, (yAnterior - row), (xAnterior - col), _FlowDirection.Cellsize)) * 1000 'Incrementa o comprimento acumulado

                        'Atribui as novas posições
                        yAnterior = row
                        xAnterior = col
                    Loop

                    SegmentNumber += 1 'Indica que deve ser incrementado o número de segmentos
                    Exit Do 'Sai fora do loop geral

                End If

                'If _CellAnalyzed(row, col) = True Then Exit Do 'Se a célula já foi analisada, passa para a próxima junção

                _CellAnalyzed(row, col) = True 'Indica que a célula já foi analisada
                _StrSeg.Dados(row, col) = SegmentNumber  'enumera o trecho segmentado

                'Pega as coordanadas do ponto
                yLat = _FlowDirection.YllCorner + (_FlowDirection.Linhas - 1 - row) * _FlowDirection.Cellsize + (_FlowDirection.Cellsize / 2)
                xLon = _FlowDirection.XllCorner + (col * _FlowDirection.Cellsize) + (_FlowDirection.Cellsize / 2)

                totalLength += (IPHDataManagement.SpatialAnalyst.CellLength(yLat, xLon, (yAnterior - row), (xAnterior - col), _FlowDirection.Cellsize)) * 1000 'Incrementa o comprimento acumulado
                If totalLength > _minLength Then 'Se comprimento acumulado for maior do que o limiar estabelecido
                    SegmentNumber += 1 'Aumenta a numeração dos segmentos
                    totalLength = 0 'Reinicia o comprimento acumulado
                End If

            Loop
nextJunction:

        Next

    End Sub

    Private Function countCells(ByVal number As Integer) As Long
        Dim counter As Long = 0

        For a = 0 To _StrSeg.Linhas - 1
            For b = 0 To _StrSeg.Colunas - 1
                If _StrSeg.Dados(a, b) = number Then counter += 1
            Next
        Next
        Return counter

    End Function

    'Move célula rio acima
    Private Sub MoveToUpstream(ByVal Yc As Int16, ByVal Xc As Int16, ByRef rowUpstr As Int16, ByRef colUpstr As Int16, ByVal ForceHeadwater As Boolean)

        Dim xi, yi As Int16
        Dim MaxUpstreamArea As Single = 0

        rowUpstr = Yc : colUpstr = Xc

        For y = -1 To 1
            yi = (Yc + y)
            For x = -1 To 1
                xi = (Xc + x)

                If x <> 0 OrElse y <> 0 Then 'Evita a análise da própria célula (nó central)
                    If xi >= 0 AndAlso yi >= 0 AndAlso xi < _FlowDirection.Colunas AndAlso yi < _FlowDirection.Linhas Then 'Evita sair dos limites do raster

                        If _StrSeg.Dados(yi, xi) > 0 OrElse ForceHeadwater = True Then 'Se estiver em cima da rede de drenagem

                            If FlowDirection.RelativeIncipientFlowDirection(yi, Yc, xi, Xc) = _FlowDirection.Dados(yi, xi) Then
                                'Verifica qual a célula com a maior área contribuinte
                                If _FlowAcc.Dados(yi, xi) > MaxUpstreamArea Then
                                    MaxUpstreamArea = _FlowAcc.Dados(yi, xi)
                                    rowUpstr = yi
                                    colUpstr = xi
                                End If
                            End If
                        End If
                    End If
                End If
            Next
        Next

    End Sub

    'Verifica se algum vizinho está apontando FD para
    Private Sub IdentifyAndMarkJunctions(ByVal Yc As Int16, ByVal xc As Int16)

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

                        If _StrSeg.Dados(yi, xi) > 0 Then 'Somente se a célula faz parte da rede de drenagem principal
                            If FlowDirection.RelativeIncipientFlowDirection(yi, Yc, xi, xc) = _FlowDirection.Dados(yi, xi) Then
                                nPointingCells += 1
                            End If
                        End If
                    End If
                End If
            Next
        Next

        If nPointingCells > 1 Then 'Caso tenha mais que uma célula apontando para a célula central
            For y = -1 To 1
                yi = (Yc + y)
                For x = -1 To 1
                    xi = (xc + x)

                    If x <> 0 OrElse y <> 0 Then 'Evita a análise da própria célula (nó central)
                        If xi >= 0 AndAlso yi >= 0 AndAlso xi < _FlowDirection.Colunas AndAlso yi < _FlowDirection.Linhas Then 'Evita sair dos limites do raster
                            If _StrSeg.Dados(yi, xi) > 0 Then 'Somente se a célula faz parte da rede de drenagem principal
                                If FlowDirection.RelativeIncipientFlowDirection(yi, Yc, xi, xc) = _FlowDirection.Dados(yi, xi) Then

                                    If _CellAnalyzed(yi, xi) = False Then 'Adiciona a célula somente se ela não tenha sido adicionada ainda
                                        _NumberOfJunctionCells += 1
                                        If _NumberOfJunctionCells > _GuessJunctionNumber Then 'Caso a matriz precise ser maior do que o chute inicial
                                            _GuessJunctionNumber = _GuessJunctionNumber * 2 'Dobra o tamanho da matriz e preserva os itens
                                            ReDim Preserve _JunctionCells(_GuessJunctionNumber)
                                            ReDim Preserve _JunctionCellsAreas(_GuessJunctionNumber)
                                        End If

                                        _JunctionCells(_NumberOfJunctionCells) = New JunctionCell(yi, xi) 'Adiciona o exutório do contribuinte
                                        _JunctionCellsAreas(_NumberOfJunctionCells) = _FlowAcc.Dados(yi, xi) 'Adiciona a área acumulada para as células
                                        _CellAnalyzed(yi, xi) = True 'Marca a célula adicionada como já sendo analisada
                                    End If

                                End If
                            End If
                        End If
                    End If
                Next
            Next
        End If

    End Sub

    'Escreve os dados da matrix dos trechos segmentados
    Public Sub WriteSegmentedStreams(ByVal arquivo As String)
        _StrSeg.WriteData(arquivo)
    End Sub

    ''' <summary>
    ''' Segmenta a rede por um comprimento máximo, não levando em consideração as junções da rede.
    ''' </summary>
    Public Sub DoubleSegmentByLength()

        ReDim _CellAnalyzed(_StrSeg.Linhas, _StrSeg.Colunas) 'Reinicializa o vetor das células analizadas
        Dim SegmentNumber As Integer = 1
        Dim VariableSegmentNumber As Integer = 0
        Dim xAnterior, yAnterior As Int16
        Dim row, col As Int16
        Dim yLat, xLon As Single
        Dim totalLength As Single = 0

        For nJunction = _NumberOfJunctionCells To 0 Step -1 'Faz da maior área para a menor

            totalLength = 0
            col = _JunctionCells(nJunction).x
            row = _JunctionCells(nJunction).y

            'Caso não tiver sido atribuído previamente o valor do segmento para a célula em questão 
            If _JunctionCells(nJunction).SegmentValue = 0 Then
                _JunctionCells(nJunction).SegmentValue = SegmentNumber
                VariableSegmentNumber = SegmentNumber
            Else
                totalLength = _JunctionCells(nJunction).ActualLength
                VariableSegmentNumber = _JunctionCells(nJunction).SegmentValue
            End If

            If _CellAnalyzed(row, col) = True Then GoTo nextjunction 'Se a célula já foi analisada, passa para a próxima junção

            Do 'Faça até chegar na cabeceira do rio mais longo
                xAnterior = col
                yAnterior = row

                _CellAnalyzed(row, col) = True
                _StrSeg.Dados(row, col) = VariableSegmentNumber  'enumera o trecho segmentado

                MoveToUpstream_DoubleSegmented(yAnterior, xAnterior, row, col, False, totalLength, VariableSegmentNumber) 'Move em direção à célula à montante com maior área

                If row = yAnterior AndAlso col = xAnterior Then 'Significa que chegou na célula de cabeceira

                    Do While totalLength < _minLength 'Faça até atingir o limite de extensão
                        MoveToUpstream_DoubleSegmented(yAnterior, xAnterior, row, col, True, totalLength, VariableSegmentNumber) 'Move em direção à célula à montante com maior área, quebrando a barreira do stream definition

                        If row = yAnterior AndAlso col = xAnterior Then Exit Do 'Significa que chegou na célula mais remota da bacia, vai para a próxima junção

                        _CellAnalyzed(row, col) = True 'Marca a célula como analisada
                        _StrSeg.Dados(row, col) = _StrSeg.Dados(yAnterior, xAnterior) 'enumera o trecho segmentado de acordo com o trecho anterior

                        totalLength += (IPHDataManagement.SpatialAnalyst.CellLength(yLat, xLon, (yAnterior - row), (xAnterior - col), _FlowDirection.Cellsize)) * 1000 'Incrementa o comprimento acumulado

                        'Atribui as novas posições
                        yAnterior = row
                        xAnterior = col
                    Loop

                    SegmentNumber += 1 'Indica que deve ser incrementado o número de segmentos
                    VariableSegmentNumber = SegmentNumber
                    Exit Do 'Sai fora do loop geral

                End If

                _CellAnalyzed(row, col) = True 'Indica que a célula já foi analisada
                _StrSeg.Dados(row, col) = VariableSegmentNumber  'enumera o trecho segmentado

                'Pega as coordanadas do ponto
                yLat = _FlowDirection.YllCorner + (_FlowDirection.Linhas - 1 - row) * _FlowDirection.Cellsize + (_FlowDirection.Cellsize / 2)
                xLon = _FlowDirection.XllCorner + (col * _FlowDirection.Cellsize) + (_FlowDirection.Cellsize / 2)

                totalLength += (IPHDataManagement.SpatialAnalyst.CellLength(yLat, xLon, (yAnterior - row), (xAnterior - col), _FlowDirection.Cellsize)) * 1000 'Incrementa o comprimento acumulado
                If totalLength > _minLength Then 'Se comprimento acumulado for maior do que o limiar estabelecido
                    SegmentNumber += 1 'Aumenta a numeração dos segmentos
                    VariableSegmentNumber = SegmentNumber
                    totalLength = 0 'Reinicia o comprimento acumulado
                End If

            Loop
nextJunction:

        Next

    End Sub

    'Move célula rio acima
    Private Sub MoveToUpstream_DoubleSegmented(ByVal Yc As Int16, ByVal Xc As Int16, ByRef rowUpstr As Int16, ByRef colUpstr As Int16, ByVal ForceHeadwater As Boolean, ByVal ActualLength As Single, ByVal segmentValue As Integer)

        Dim xi, yi, nPointCell As Int16
        Dim MaxUpstreamArea As Single = 0
        Dim cellNum As Long
        Dim restartVerification As Boolean = False
        rowUpstr = Yc : colUpstr = Xc

restartPosition:
        nPointCell = 0

        For y = -1 To 1
            yi = (Yc + y)
            For x = -1 To 1
                xi = (Xc + x)

                If x <> 0 OrElse y <> 0 Then 'Evita a análise da própria célula (nó central)
                    If xi >= 0 AndAlso yi >= 0 AndAlso xi < _FlowDirection.Colunas AndAlso yi < _FlowDirection.Linhas Then 'Evita sair dos limites do raster

                        If _StrSeg.Dados(yi, xi) > 0 OrElse ForceHeadwater = True Then 'Se estiver em cima da rede de drenagem

                            If FlowDirection.RelativeIncipientFlowDirection(yi, Yc, xi, Xc) = _FlowDirection.Dados(yi, xi) Then

                                nPointCell += 1

                                If restartVerification = True Then 'Se identificou que existe uma junção, atribui os dados para as células de exutório
                                    cellNum = FindCellPosition(yi, xi)
                                    If Not cellNum < 0 Then
                                        _JunctionCells(cellNum).ActualLength = ActualLength
                                        _JunctionCells(cellNum).SegmentValue = segmentValue
                                    End If
                                End If

                                'Verifica qual a célula com a maior área contribuinte
                                If _FlowAcc.Dados(yi, xi) > MaxUpstreamArea Then
                                    MaxUpstreamArea = _FlowAcc.Dados(yi, xi)
                                    rowUpstr = yi
                                    colUpstr = xi
                                End If
                            End If
                        End If
                    End If
                End If
            Next
        Next

        If nPointCell > 1 AndAlso restartVerification = False Then
            restartVerification = True
            GoTo restartPosition
        End If

    End Sub

    ''' <summary>
    ''' Identifica a posição da célula que corresponde à cordenada indicada
    ''' </summary>
    ''' <param name="y"></param>
    ''' <param name="x"></param>
    Private Function FindCellPosition(ByVal y As Int16, ByVal x As Int16) As Long

        For i = 0 To _NumberOfJunctionCells - 1
            If _JunctionCells(i).x = x AndAlso _JunctionCells(i).y = y Then
                Return i
            End If
        Next

        Return -1 'Caso não encontrar a célula no meio das demais
        'Throw New Exception("Junction cells were not found!")

    End Function

End Class

Public Class JunctionCell
    Inherits Cells

    Public ActualLength As Single
    Public SegmentValue As Integer

    Public Sub New(ByVal y As Int16, ByVal x As Int16)
        MyBase.New(y, x)
    End Sub

End Class

