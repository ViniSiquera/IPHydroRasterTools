'*******************************************************************
'REMOÇÃO DE DEPRESSÕES
'Criado por Vinícius Alencar Siqueira - 20/01/2014, 'CÓDIGO MHS (Modified Heuristic Search) - v1.4
'Baseado no Artigo de Hou et al (2011) - Automatic Extraction of Drainage Networks from DEMs Base on Heuristic Search. Journal of Software, Vol. 6, nº 8
'*******************************************************************
Imports IPHDataManagement

''' <summary>
''' Classe utilizada ara remover depressões - MHS, HS e PFS
''' </summary>
Public Class HeuristicSinkRemoval

    Private _MDE, _FlowDirection As RasterInteger 'Rasters
    Private _MatrizD8(8) As Single 'Matriz dos 8 vizinhos
    Private _Array() As Cells  'Matriz de células sem Direcao de Fluxo - células com depressão
    Private _tamanhoJanela As Integer = 5 'Tamanho da janela é a dimensão para N x N do método orinal de Hou et al (2011); 
    Private _MaxOpenList, _MaxClosedList As Integer
    Private _WeightFuncG As Single 'Peso da função de custo 

    Private _directionsX() As Short = {-1, 0, 1, 0, -1, -1, 1, 1} 'Matriz de direções em X
    Private _directionsY() As Short = {0, -1, 0, 1, 1, -1, -1, 1} 'Matriz de direções em Y

    Private _OpenList() As HeuristicCell 'Conjunto das células candidatas
    Private _ClosedList() As HeuristicCell 'Conjunto das células selecionadas
    Private _ClosedListBoolean(,), _OpenListBoolean(,) As Boolean
    Private _ClosedListPosition(), _OpenListPosition() As Long
    Private _TraceBackMatrix(,) As Integer
    Private _ForcedOutlets(,) As Boolean
    Private _AlgorithmMode As ProcessingMode 'Método de remoção de depressões

    Public percent As Single 'Percentual de conclusão
    Public erro As Boolean = True

    'Enumeradores para modo de processamento
    Public Enum ProcessingMode

        MHS = 0 'Utiliza o modo Modified Heuristic Search
        HS = 1 'Utiliza o modo Heuristic Search
        PFS = 2 'Utiliza o modo PFS 

    End Enum

#Region "Propriedades e Métodos"

    ''' <summary>
    ''' Retorna ou atribui o tamanho da janela que se move em torno da célula com depressão (somente modo Heuristic Search)
    ''' </summary>
    Public Property TamanhoJanela() As Integer
        Get
            Return _tamanhoJanela
        End Get
        Set(ByVal value As Integer)
            _tamanhoJanela = value
        End Set
    End Property

    ''' <summary>
    ''' Retorna ou atribui o modo de remoção de depressões
    ''' </summary>
    Public Property ProcessMode As ProcessingMode
        Get
            Return _AlgorithmMode
        End Get
        Set(ByVal value As ProcessingMode)
            _AlgorithmMode = value
        End Set
    End Property

    ''' <summary>
    ''' Retorna o MDE original ou modificado pelo processo
    ''' </summary>
    Public Function GetMDEmatrix() As RasterInteger
        Return _MDE.CopyData
    End Function

    ''' <summary>
    ''' Retorna o MDE original ou modificado pelo processo
    ''' </summary>
    Public Function GetFlowDirectionMatrix() As RasterInteger
        Return _FlowDirection.CopyData
    End Function

#End Region

    ''' <summary>
    ''' Cria uma nova instância da classe Removedora de depressões
    ''' </summary>
    ''' <param name="maxOpenList">Descrição: Número máximo de elementos na lista de células candidatas.</param>
    ''' <param name="maxClosedList">Descrição: Número máximo de elementos na lista de células selecionadas.</param>
    ''' <param name="WeightFunctionG">Descrição: Peso para a função de custo G(i)</param>
    ''' <param name="ProcessingAlgorithm">Descrição: Modo de processamento para a remoção de depressões.</param>
    ''' <remarks></remarks>
    Public Sub New(ByVal maxOpenList As Integer, ByVal maxClosedList As Integer, ByVal WeightFunctionG As Single, ByVal ProcessingAlgorithm As HeuristicSinkRemoval.ProcessingMode)
        Me._MDE = New RasterInteger()
        _MaxClosedList = maxClosedList
        _MaxOpenList = maxOpenList
        _WeightFuncG = WeightFunctionG
        _AlgorithmMode = ProcessingAlgorithm
    End Sub

    'Lê um arquivo de MDE, especificando o local
    Public Sub ReadMDEdata(ByVal Local As String)

        Me._MDE.ReadData(Local)
        Me._FlowDirection = New RasterInteger(_MDE.Linhas, _MDE.Colunas, _MDE.XllCorner, _MDE.YllCorner, _MDE.Cellsize, _MDE.NoDataValue)
        ReDim _ForcedOutlets(_MDE.Linhas - 1, _MDE.Colunas - 1)

    End Sub

    ''' <summary>
    ''' Rotina para removção das depressões do MDE.
    ''' </summary>
    Public Sub RemoveSinks()

        erro = True 'Flag que indica se ocorreu algum erro no processo: inicialmente é dado como VERDADEIRO

        ReDim Me._Array((_MDE.Linhas * _MDE.Colunas) - 1) 'Redimensiona inicialmente o vetor de células com depressão

        FlowDirectionAtBounds() 'Calcula o Flow Direction na borda
        CalcularFlowDirection() 'Calcula o Flow Direction de toda a matriz
        FillDepressions(0) 'Realiza uma operação de Fill, atribuindo a cota do vizinho com elevação mais próxima

        'Lista de nós, candidatos a serem selecionados para um possível trajeto de mínimo custo
        ReDim _OpenList(_MaxOpenList - 1)
        ReDim _OpenListPosition(_MaxOpenList - 1)
        ReDim _OpenListBoolean(_MDE.Linhas - 1, _MDE.Colunas - 1)

        'Lista de nós que já foram selecionados e checados
        ReDim _ClosedList(_MaxClosedList - 1)
        ReDim _ClosedListPosition(_MaxClosedList - 1)
        ReDim _ClosedListBoolean(_MDE.Linhas - 1, _MDE.Colunas - 1)

        'Matriz que indica o caminho de volta, a partir do outlet
        ReDim _TraceBackMatrix(_MDE.Linhas - 1, _MDE.Colunas - 1)

        'Remove as depressões
        RemoveDepressions()
        'Libera a memória
        ReleaseMemory()
        'Remove os ruídos do MDE final de acordo com as direções de fluxo
        AdjustDemFinalElevations()

        erro = False 'Se chegar até aqui, então não ocorreram erros

    End Sub

    'Remove as depressões utilizando uma matriz 'n x n' para busca em torno da depressão espúria
    Private Sub RemoveDepressions()

        Dim intClosedList As Integer = 0 'Número de elementos da closed list, que será variável
        Dim intOpenList As Integer = 0 'Número de elementos da open list, que será variável
        Dim intClosedListForcedBound As Integer

        Dim celulas As Long = Me._MDE.TotalDeCelulas
        Dim OutletFound As Boolean = False
        Dim nDepressions As Long = _Array.Count
        Dim StartElevation, EndElevation As Int16
        Dim Xposition, Yposition As Integer

        For iArray = 0 To nDepressions - 1 'Faça para todas as depressões encontradas

            OutletFound = False 'Torna falso o encontro da saída
            Xposition = _Array(iArray).x 'Identifica a posição X da depressão
            Yposition = _Array(iArray).y 'Identifica a posição Y da depressão

            If _FlowDirection.Dados(Yposition, Xposition) = 0 Then 'Pode ser que o Flow Direction já tenha sido calculado em uma das etapas abaixo!

                StartElevation = _MDE.Dados(Yposition, Xposition) 'Identifica a elevação da depressão selecionada
                AddDepressionToOpenList(Yposition, Xposition, intOpenList) 'Adiciona inicialmente a célula com a depressão na open list, para ser analisada
                PushCellToClosedList(0, intClosedList) 'Move a célula da depressão, da open list para a closed list
                AddNeighboursToOpenList(Yposition, Xposition, intOpenList) 'Adiciona os vizinhos da célula com depressão na open list, de forma que estas sejam candidatas à solução do problema

                Do Until OutletFound = True 'Faça até que achar uma célula em que pode verter a água

                    For z = 0 To intOpenList - 1 'Calcula o valor heurístico de cada uma das células da openlist
                        If _OpenList(z) IsNot Nothing Then
                            With _OpenList(z)

                                'Identifica o método que será utilizado para remoção de depressões
                                If _AlgorithmMode = 0 Then 'Algoritmo MHS
                                    .CostFunction = ModifiedHeuristicValue(.y, .x, StartElevation, (.y - Yposition), (.x - Xposition), _WeightFuncG)
                                ElseIf _AlgorithmMode = 1 Then 'Algoritmo HS
                                    .CostFunction = HeuristicValue(.y, .x, StartElevation)
                                ElseIf _AlgorithmMode = 2 Then 'Algoritmo PFS
                                    .CostFunction = PFSValue(.y, .x, StartElevation)
                                End If

                            End With
                        End If
                    Next

                    Dim iNextCell As Integer = EnumMinHeuristicInfo(intOpenList) 'Identifica a célula com o maior valor heurístico na open list
                    EndElevation = _MDE.Dados(_OpenList(iNextCell).y, _OpenList(iNextCell).x) 'Elevação inicial da depressão

                    OutletFound = IsOutlet(EndElevation, StartElevation, _OpenList(iNextCell).y, _OpenList(iNextCell).x) 'Verifica se encontrou a saída para o problema

                    'Força a saída de água no limite mais próximo da borda, caso exceder um numero pré-determinado de células
                    If intClosedList = _MaxClosedList Then

                        intClosedListForcedBound = _MaxClosedList
                        ForceBoundOutlet(_ClosedList(0).y, _ClosedList(0).x, _MaxClosedList, intClosedList)
                        OutletFound = True

                        GoTo endMovingCells
                    End If

                    If OutletFound = False Then
                        'Se não encontrou um ponto para verter a água, continua realizando a análise 
                        With _OpenList(iNextCell)

                            PushCellToClosedList(iNextCell, intClosedList) 'Move a célula para a closed list e retira a mesma da open list
                            AddNeighboursToOpenList(.y, .x, intOpenList) 'Adiciona os vizinhos da célula selecionada (de máximo valor heurístico) na open list, para serem avaliados

                            'Atualiza a posição da célula atual
                            Yposition = .y
                            Xposition = .x
                        End With
                    Else

                        'Termina a operação, estabelece o caminho e reseta as listas para a próxima depressão
                        PushCellToClosedList(iNextCell, intClosedList) 'Move a célula para a closed list e retira a mesma da open list
endMovingCells:
                        Breaching(intClosedList) 'Modifica o MDE para o caminho encontrado

                        'Retorna o número de elementos original para resetar a lista
                        If intClosedListForcedBound = _MaxClosedList Then intClosedList = intClosedListForcedBound : intClosedListForcedBound = 0

                        ResetAllList(intClosedList, intOpenList)
                        intClosedList = 0
                        intOpenList = 0

                    End If
                Loop
            End If
        Next

    End Sub

    'Adiciona a célula com a depressão na openlist para ser analisada
    Private Sub AddDepressionToOpenList(ByVal Yc As Int16, ByVal Xc As Int16, ByRef nElementos As Integer)

        _OpenList(nElementos) = New HeuristicCell(Yc, Xc) 'Coloca a célula na open list
        _OpenListBoolean(Yc, Xc) = True 'Aciona o Flag para indicar que a célula está na open list
        _OpenList(nElementos).RelParentX = 0
        _OpenList(nElementos).RelParentY = 0
        _OpenListPosition(nElementos) = (Yc * _MDE.Colunas) + Xc 'Identifica a posição da célula na matriz (na forma de vetor)

        nElementos += 1
    End Sub

    'Adiciona os elementos vizinhos na Open List, retornando o número de elementos adicionados
    Private Sub AddNeighboursToOpenList(ByVal Yc As Int16, ByVal Xc As Int16, ByRef nElementos As Integer)

        'Yc = Y da célula central
        'Xc = X da célula central

        Dim posX, posY As Int16

        'Utiliza uma matriz '3 x 3' em torno da célula central, identificando as células vizinhas e fazendo um somatório das elevações
        For y = -1 To 1
            posY = (Yc + y)
            For x = -1 To 1
                posX = (Xc + x)

                If posX >= 0 AndAlso posY >= 0 AndAlso posX < _MDE.Colunas AndAlso posY < _MDE.Linhas Then 'Evita que o vizinho saia fora dos limites da grade
                    If _ClosedListBoolean(posY, posX) = False AndAlso _OpenListBoolean(posY, posX) = False Then 'Somente adiciona se não estiver nem na open list e nem na closed list
                        If y <> 0 Or x <> 0 Then 'Somente adiciona se não for o ponto central
                            _OpenList(nElementos) = New HeuristicCell(posY, posX)

                            _OpenList(nElementos).RelParentX = Xc - posX 'Estabelece pointers relativos para o o nó de origem
                            _OpenList(nElementos).RelParentY = Yc - posY

                            _OpenListBoolean(posY, posX) = True 'Aciona o flag indicando que a célula está na open list
                            _OpenListPosition(nElementos) = (posY * _MDE.Colunas) + posX 'Indica a posição do ponto da open list
                            nElementos += 1

                        End If
                    End If
                End If
            Next
        Next

    End Sub

    'Realiza um ajuste final no MDE utilizando o flow direction acertado
    Private Sub AdjustDemFinalElevations()

        Dim CheckedNodes(,), nextCell As Boolean
        ReDim CheckedNodes(_MDE.Linhas - 1, _MDE.Colunas - 1)
        Dim PreviousElevation, NextElevation As Int16

        Dim line, column As Int16
        nextCell = False

        'Fazer para toda a grade
        For i = 0 To _MDE.Linhas - 1
            For j = 0 To _MDE.Colunas - 1

                line = i
                column = j

                PreviousElevation = _MDE.Dados(line, column) 'Armazena a elevação da célula atual

                If CheckedNodes(line, column) = False Then 'Se a célula atual não tiver sido checada ainda
                    Do
                        MoveToFlowDirection(_FlowDirection.Dados(line, column), line, column) 'Move para a célula de jusante, de acordo com o flow direction
                        If line < 0 OrElse column < 0 OrElse line > (_MDE.Linhas - 1) OrElse column > (_MDE.Colunas - 1) Then Exit Do 'Evita sair fora dos limites
                        If CheckedNodes(line, column) = True Then Exit Do 'Se a célula atual (de jusante) já tiver sido checada, sai do loop

                        NextElevation = _MDE.Dados(line, column) 'Armazena a elevação da célula a jusante

                        If NextElevation > PreviousElevation Then _MDE.Dados(line, column) = PreviousElevation 'Se a célula a jusante tiver uma cota superior à anterior, iguala a da anterior
                        CheckedNodes(line, column) = True 'Assume a célula como checada
                        PreviousElevation = NextElevation 'A nova elevação da célula

                        If line < 0 OrElse column < 0 OrElse line > (_MDE.Linhas - 1) OrElse column > (_MDE.Colunas) Then Exit Do
                    Loop
                End If
            Next
        Next
    End Sub

    'Faz um procedimento de "cavar" o MDE, utilizando o caminho encontrado
    Private Sub Breaching(ByVal ClosedListCount As Integer)

        'identifica as elevações inicial e final da pathlist
        Dim InitElevation As Int16 = _MDE.Dados(_ClosedList(0).y, _ClosedList(0).x)
        Dim FinalElevation As Int16 = _MDE.Dados(_ClosedList(ClosedListCount - 1).y, _ClosedList(ClosedListCount - 1).x)

        'Evita que o programa utilize o valor do NoDATA como elevação final, o que pode comprometer o resultado
        If FinalElevation = _MDE.NoDataValue Then FinalElevation = InitElevation

        If FinalElevation > InitElevation Then
            FinalElevation = InitElevation 'Pode ocorrer em situações de limite do MDE, nesse caso, força o decréscimo na cota
            _MDE.Dados(_ClosedList(ClosedListCount - 1).y, _ClosedList(ClosedListCount - 1).x) = FinalElevation
        End If

        Dim nCells, xParent, yParent As Integer
        Dim FoundStartNode As Boolean = False
        Dim enumerator As Integer = ClosedListCount - 1

        'Identifica o nº de células no caminho do outlet até a depressão
        Do While FoundStartNode = False
            xParent = _ClosedList(enumerator).x + _ClosedList(enumerator).RelParentX 'Acha a coordenada do parent node
            yParent = _ClosedList(enumerator).y + _ClosedList(enumerator).RelParentY

            If _ClosedList(enumerator).RelParentX = 0 AndAlso _ClosedList(enumerator).RelParentY = 0 Then FoundStartNode = True

            enumerator = _TraceBackMatrix(yParent, xParent) 'Identifica o enumerador do nó de coordenada x,y
            nCells += 1
        Loop

        Dim Incremental As Single = (InitElevation - FinalElevation) / nCells 'Calcula o incremento para escavação entre a elevação do outlet e da depressão

        'Identifica o flow Direction para cada uma das células do pathlist e ajusta as respectivas cotas do MDE
        enumerator = ClosedListCount - 1

        For i = 1 To nCells - 1  'A primeira célula eu não posso modificar, pois é o outlet; a ´´ultima célula, que é o ponto de saída, também não modifico
            With _ClosedList(enumerator)
                xParent = .x + .RelParentX 'Acha a coordenada do parent node
                yParent = .y + .RelParentY
                _FlowDirection.Dados(yParent, xParent) = RelativeIncipientFlowDirection(yParent, .y, xParent, .x) 'Atribuo o novo flow direction

                'Só atribuo a cota se ela for menor do que a existente
                If _MDE.Dados(yParent, xParent) > FinalElevation + Math.Round((Incremental * i), 0) Then
                    _MDE.Dados(yParent, xParent) = FinalElevation + Math.Round((Incremental * i), 0) 'Atribuo a nova cota
                End If

            End With
            enumerator = _TraceBackMatrix(yParent, xParent) 'Identifica o enumerador do nó de coordenada x,y
        Next
    End Sub

    'Retorna o enumerador da célula com o maior valor heurístico
    Private Function EnumMinHeuristicInfo(ByVal nOpenlist As Integer) As Integer

        Dim value As Single = 99999
        Dim enumerator As Integer = -1

        For I = 0 To nOpenlist - 1
            If _OpenList(I) IsNot Nothing Then
                If _OpenList(I).CostFunction < value Then value = _OpenList(I).CostFunction : enumerator = I
            End If
        Next

        Return enumerator

    End Function

    'Preenche a depressão até a cota do vizinho mais próximo, de acordo com um treshold pré definido
    Private Sub FillDepressions(ByVal Difference_Height As Integer)

        Dim Xc, Yc As Int16
        Dim nDepressions As Long = _Array.Count
        Dim elevation As Int16
        Dim lowerBound As Int16

        For i = 0 To nDepressions - 1

            Yc = _Array(i).y
            Xc = _Array(i).x

            elevation = _MDE.Dados(Yc, Xc) 'Assume a elevação da célula '//MODIFICADO EM 21/11/2013
            lowerBound = 9999

            'Verifica qual é o vizinho com menor elevação
            For yi = -1 To 1
                For xi = -1 To 1
                    If yi <> 0 OrElse xi <> 0 Then 'exclui a verificação para o ponto central
                        If Not _MDE.Dados(Yc + yi, Xc + xi) = _MDE.NoDataValue Then 'Não calcula a elevação for NODATA
                            If _MDE.Dados(Yc + yi, Xc + xi) < lowerBound Then lowerBound = _MDE.Dados(Yc + yi, Xc + xi)
                        End If
                    End If
                Next
            Next

            'Se a diferença entre a elevação de um vizinho e a elevação da depressão é maior do que um limite pré definido, aumenta a cota até este ponto
            If (lowerBound - elevation) >= Difference_Height Then _MDE.Dados(Yc, Xc) = lowerBound
            'If (lowerBound - elevation) > Difference_Height Then _MDE.Dados(Yc, Xc) = lowerBound - Difference_Height '\\\MODIFICADO EM 19/11/2013, por Vinicius
        Next

    End Sub

    'Força a saída pelas bordas da parede
    Private Sub ForceBoundOutlet(ByVal Yc As Int16, ByVal Xc As Int16, ByVal MaxClosedListElements As Integer, ByRef nActualClosedListElements As Integer)

        Dim elements As Integer = -1

        'Verifica, dentre todas as células da closed list, a primeira célula que for de borda
        For w = 0 To MaxClosedListElements - 1
            If _ClosedList(w).x = 0 OrElse _ClosedList(w).y = 0 OrElse _ClosedList(w).x = (_MDE.Colunas - 1) OrElse _ClosedList(w).y = (_MDE.Linhas - 1) Then
                elements = (w + 1) : Exit For
            End If
        Next

        If elements = -1 Then 'Caso não houver nenhuma célula de borda, dispara o erro
            Throw New Exception("O número máximo de elementos da closed list não foi suficiente para encontrar o trecho de rio para o ponto X,Y = (" & Xc & "," & Yc & ").")
        End If

        nActualClosedListElements = elements

    End Sub

    'Retorna o valor da função heurística para uma célula localizada na posição Xc, Yc
    Private Function HeuristicValue(ByVal Yc As Int16, ByVal Xc As Int16, ByVal Es As Int16) As Single

        'PosY = posição Y da célula a ser somada 
        'PosX = posição X da célula a ser somada
        'Ei = valor da elevação na célula de indice 'i'
        'Es = valor da elevação na célula inicial (starting node)

        Dim nCelulas As Integer = 0
        Dim Ei As Int16 = _MDE.Dados(Yc, Xc)
        Dim elevation As Int16
        Dim posX, posY As Int16

        Dim n As Integer = Math.Floor(_tamanhoJanela / 2) 'Divide dimensão por 2 e arredonda para o inteiro inferior; se n = 5, vai de -2 até 2
        Dim soma As Long = 0

        Dim Gi As Int16 = Ei - Es 'valor da função G(i) -> diferença de cota entre o nó inicial e o de índice 'i'
        Dim Hi As Single = 0

        'Utiliza uma matriz 'n x n' em torno da célula central (porém incluindo a mesma), identificando as células vizinhas e fazendo um somatório das elevações
        For y = -n To n
            posY = (Yc + y)
            For x = -n To n
                posX = (Xc + x)

                If posX >= 0 And posY >= 0 And posX < _MDE.Colunas And posY < _MDE.Linhas Then 'Evita que o vizinho saia fora dos limites da grade
                    elevation = _MDE.Dados((Yc + y), (Xc + x))
                    soma += elevation
                    nCelulas += 1
                End If
            Next
        Next

        If Not nCelulas = 0 Then Hi = (soma / nCelulas) 'Valor da função H(i), evita divisão por 0 (modificado em relação à original)

        Return Gi + Hi 'valor da função heurística F(i) = H(i) + G(i)

    End Function

    'Verifica se chegou em algum ponto de exutório
    Private Function IsOutlet(ByVal Ei As Int16, ByVal Es As Int16, ByVal posY As Int16, ByVal posX As Int16) As Boolean
        If Ei = _MDE.NoDataValue Then 'Se chegar no NODATA, retorna true
            Return True
        ElseIf Ei < Es Then 'Se a diferença de cota do ponto final for menor ou igual a 2 e estiver na borda do MDE, aceita como exutório
            Return True
        ElseIf (Ei - Es) <= 2 Then
            If posX = 0 OrElse posY = 0 OrElse (posX = _MDE.Colunas - 1) OrElse (posY = _MDE.Linhas - 1) Then
                Return True
            End If
        End If
        Return False
    End Function

    'Retorna o valor da função heurística modificada, para uma célula cujo nó pai (parent node) está localizado na posição Xc, Yc
    'Deve ser informada a posição relativa xRel, yRel em relação ao nó pai.
    Private Function ModifiedHeuristicValue(ByVal Yc As Int16, ByVal Xc As Int16, ByVal Es As Int16, ByVal RelPosY As Int16, ByVal RelPosX As Int16, ByVal Wg As Single) As Single

        'Ei = valor da elevação na célula de indice 'i'
        'Es = valor da elevação na célula inicial (starting node)

        Dim nCelulas As Integer = 0
        Dim Ei As Int16 = _MDE.Dados(Yc, Xc)
        Dim PosX, PosY As Int16
        Dim dy1, dy2, dx1, dx2 As Int16

        Dim soma As Long = 0

        Dim Gi As Int16 = Ei - Es 'valor da função G(i) -> diferença de cota entre o nó inicial e o de índice 'i'
        Dim Hi As Int16 = 0

        'Identifica os limites da variação em y
        Select Case RelPosY
            Case -1
                dy1 = -2 : dy2 = 0
            Case 0
                dy1 = -1 : dy2 = 1
            Case 1
                dy1 = 0 : dy2 = 2
        End Select

        'Identifica os limites da variação em x
        Select Case RelPosX
            Case -1
                dx1 = -2 : dx2 = 0
            Case 0
                dx1 = -1 : dx2 = 1
            Case 1
                dx1 = 0 : dx2 = 2
        End Select

        'Utiliza uma matriz 'n x n' nos limites das células em torno da célula central, fazendo um somatório das elevações
        For y = dy1 To dy2
            PosY = (Yc + y)
            For x = dx1 To dx2
                PosX = (Xc + x)

                If PosX >= 0 And PosY >= 0 And PosX < _MDE.Colunas And PosY < _MDE.Linhas Then 'Evita que o vizinho saia fora dos limites da grade
                    soma += _MDE.Dados((Yc + y), (Xc + x))
                    nCelulas += 1
                End If
            Next
        Next

        If Not nCelulas = 0 Then Hi = (soma / nCelulas) 'Valor da função H(i), evita divisão por 0 (modificado em relação à original)

        Return (Wg * Gi) + Hi 'valor da função heurística F(i) = H(i) + G(i) * peso

    End Function

    'Retorna o valor da função custo PFS, para uma célula localizada na posição Yc, Xc
    Private Function PFSValue(ByVal Yc As Int16, ByVal Xc As Int16, ByVal Es As Int16) As Single

        'Ei = valor da elevação na célula de indice 'i'
        'Es = valor da elevação na célula inicial (starting node)

        Dim Ei As Int16 = _MDE.Dados(Yc, Xc)
        Return (Ei - Es) 'valor da função PFS(i) -> diferença de cota entre o nó inicial e o de índice 'i'

    End Function

    'Adiciona os elementos da open list na closed list, excluindo o valor central (que deve ir para o Path list)
    Private Sub PushCellToClosedList(ByVal enumCell As Integer, ByRef nClosed As Integer)

        With _OpenList(enumCell)
            _ClosedListBoolean(.y, .x) = True 'Aciona o Flag para indicar que a célula está na closed list
            _ClosedList(nClosed) = New HeuristicCell(.y, .x) 'Coloca a célula na closed list
            _ClosedList(nClosed).RelParentX = .RelParentX
            _ClosedList(nClosed).RelParentY = .RelParentY

            _ClosedListPosition(nClosed) = (.y * _MDE.Colunas) + .x 'Identifica a posição da célula na matriz (na forma de vetor)
            _TraceBackMatrix(.y, .x) = nClosed 'guarda a posição da célula para conhecer o caminho de volta

            _OpenListBoolean(.y, .x) = False 'Retira o flag para indicar que a célula não está mais na open list
            _OpenList(enumCell) = Nothing 'Retira a célula da open list

        End With
        nClosed += 1
    End Sub

    'Libera da memória todas as matrizes utilizadas nas operações
    Private Sub ReleaseMemory()
        _OpenList = Nothing
        _ClosedList = Nothing
        _Array = Nothing
        _MatrizD8 = Nothing
        _TraceBackMatrix = Nothing
        _ClosedListBoolean = Nothing
        _ClosedListPosition = Nothing
        _OpenListBoolean = Nothing
        _OpenListPosition = Nothing
        GC.Collect()

    End Sub

    'Reinicia as open e closed lists, para a próxima depressão
    Private Sub ResetAllList(ByVal nClosedList As Integer, ByVal nOpenList As Integer)

        Dim x, y As Long

        For i = 0 To nClosedList - 1
            y = Math.Floor(_ClosedListPosition(i) / _MDE.Colunas) 'Identifica a linha
            x = _ClosedListPosition(i) - (y * _MDE.Colunas) 'Identifica a Coluna
            _ClosedListBoolean(y, x) = False 'Retira da Closed List
            _TraceBackMatrix(y, x) = 0 'Reinicia o valor da Matriz de volta
        Next

        For i = 0 To nOpenList - 1
            y = Math.Floor(_OpenListPosition(i) / _MDE.Colunas) 'Identifica a linha
            x = _OpenListPosition(i) - (y * _MDE.Colunas) 'Identifica a Coluna
            _OpenListBoolean(y, x) = False 'Retira da Open List
        Next

    End Sub

    ''' <summary>
    ''' Escreve os dados do MDE em formato ASC
    ''' </summary>
    Public Sub WriteMDEData(ByVal arquivo As String)
        _MDE.WriteData(arquivo)
    End Sub

#Region "Flow Direction"

    'Identifica as direções de fluxo e as depressões espúrias do MDE
    Private Sub CalcularFlowDirection()

        Dim countDepression As Long = 0
        Dim FDFound As Boolean

        'Calcula para toda a grade as direções de fluxo
        For y = 1 To Me._MDE.Linhas - 2
            For x = 1 To Me._MDE.Colunas - 2

                _FlowDirection.Dados(y, x) = IncipientFlowDirection(y, x)
                If _FlowDirection.Dados(y, x) = 0 Then 'Se for = 0, verifica se existe uma célula com igual cota para onde possa verter 

                    VerifyFlowDirAtBounds(y, x, FDFound)

                    If FDFound = False Then ' Se não encontrar célula para verter água, identifica uma depressão
                        _Array(countDepression) = New Cells(y, x)
                        countDepression += 1
                    End If

                    FDFound = False

                End If
            Next
        Next

        'Caso não houverem depressões, a lista não existirá
        If countDepression = 0 Then
            _Array = Nothing
        Else
            'Caso houverem depressões, redimensiona o vetor para o tamanho de células encontradas
            ReDim Preserve _Array(countDepression - 1)
        End If
    End Sub

    'Verifica se algum vizinho de mesma cota, ou inferior já possui flow direction, atribuindo a mesma em caso verdadeiro
    Private Sub VerifyFlowDirAtBounds(ByVal Yc As Int16, ByVal Xc As Int16, ByRef Validated As Boolean)

        '       'direções de apontamento para a célula central
        '       2  4  8 
        '       1    x   16
        '       128   64  32

        'Matriz de direções, no caso de apontamento - (1, 64, 16, 4, 2, 128, 32, 8)

        Validated = False
        Dim elevation As Int16 = _MDE.Dados(Yc, Xc)
        Dim yi, xi As Int16

        For index = 0 To 7 'Para os elementos da matriz de direções

            xi = Xc + _directionsX(index)
            yi = Yc + _directionsY(index)

            If _MDE.Dados(yi, xi) = elevation Then 'Somente se a célula possui cota igual àquela sendo analisada
                If _FlowDirection.Dados(yi, xi) = 0 Then Validated = True

                If Validated = True Then 'caso tenha sido encontrado algum vizinho com direção de fluxo atribuída, manda água pra ele
                    _FlowDirection.Dados(Yc, Xc) = RelativeIncipientFlowDirection(Yc, yi, Xc, xi)
                    GoTo endVerify
                End If
            End If

        Next

endVerify:

    End Sub

    'Calcula a Direção de fluxo nos limites da grade
    Private Sub FlowDirectionAtBounds()

        'Define o Flow direction dos cantos
        _FlowDirection.Dados(0, 0) = _MDE.NoDataValue
        _FlowDirection.Dados(0, _MDE.Colunas - 1) = _MDE.NoDataValue
        _FlowDirection.Dados(_MDE.Linhas - 1, 0) = _MDE.NoDataValue
        _FlowDirection.Dados(_MDE.Linhas - 1, _MDE.Colunas - 1) = _MDE.NoDataValue

        'Atribui o flow direction para fora dos limites da grade
        For y = 1 To _MDE.Linhas - 2
            _FlowDirection.Dados(y, 0) = _MDE.NoDataValue
        Next

        For y = 1 To _MDE.Linhas - 2
            _FlowDirection.Dados(y, _MDE.Colunas - 1) = _MDE.NoDataValue
        Next

        For x = 1 To _MDE.Colunas - 2
            _FlowDirection.Dados(0, x) = _MDE.NoDataValue
        Next

        For x = 1 To _MDE.Colunas - 2
            _FlowDirection.Dados(_MDE.Linhas - 1, x) = _MDE.NoDataValue
        Next

        'Define o Flow direction dos cantos
        '_FlowDirection.Dados(0, 0) = 32
        '_FlowDirection.Dados(0, _MDE.Colunas - 1) = 128
        '_FlowDirection.Dados(_MDE.Linhas - 1, 0) = 8
        '_FlowDirection.Dados(_MDE.Linhas - 1, _MDE.Colunas - 1) = 2

        'Atribui o flow direction para fora dos limites da grade
        'For y = 1 To _MDE.Linhas - 2
        '   _FlowDirection.Dados(y, 0) = 16
        'Next

        'For y = 1 To _MDE.Linhas - 2
        '   _FlowDirection.Dados(y, _MDE.Colunas - 1) = 1
        'Next

        'For x = 1 To _MDE.Colunas - 2
        '   _FlowDirection.Dados(0, x) = 64
        'Next

        'For x = 1 To _MDE.Colunas - 2
        '   _FlowDirection.Dados(_MDE.Linhas - 1, x) = 4
        'Next


    End Sub

    'Retorna a direção de fluxo de uma célula qualquer, posicionada em x,y; Função das declividades das células vizinhas
    Private Function IncipientFlowDirection(ByVal y As Integer, ByVal x As Integer) As Int16

        Dim valor As Int16 = _MDE.Dados(y, x)
        If valor = _MDE.NoDataValue Then Return _MDE.NoDataValue 'Retorna o valor do NODATA caso encontrá-lo

        'Matriz D8 - 8 vizinhos:
        '0  1  2
        '3  x  4
        '5  6  7

        'Calcula o valor da declividade para cada um dos vizinhos
        'Aquele que tiver a maior declividade é para onde irá a direção de fluxo
        'Para os sentidos 0, 2, 5 e 7 o comprimento é igual a Raiz(2)
        'Para os sentidos 1, 3, 4 e 6 o comprimento é igual a 1

        _MatrizD8(0) = (valor - _MDE.Dados(y - 1, x - 1)) / Math.Sqrt(2)
        _MatrizD8(1) = valor - _MDE.Dados(y - 1, x)
        _MatrizD8(2) = (valor - _MDE.Dados(y - 1, x + 1)) / Math.Sqrt(2)
        _MatrizD8(3) = valor - _MDE.Dados(y, x - 1)
        _MatrizD8(4) = valor - _MDE.Dados(y, x + 1)
        _MatrizD8(5) = (valor - _MDE.Dados(y + 1, x - 1)) / Math.Sqrt(2)
        _MatrizD8(6) = valor - _MDE.Dados(y + 1, x)
        _MatrizD8(7) = (valor - _MDE.Dados(y + 1, x + 1)) / Math.Sqrt(2)

        '32	64	    128
        '16	0	    1      'Configuração das direções de fluxo para o IDRISI Kilimanjaro
        '8	4	    2

        Dim max As Single = 0.01
        Dim enumMax As Int16 = -1 'Valor inicial que indica que não há direção de fluxo

        'Seleciona a maior declividade dentre as calculadas
        For i = 0 To 7
            If _MatrizD8(i) > max Then max = _MatrizD8(i) : enumMax = i
        Next

        Select Case enumMax  'Identifica o sentido conforme o número do enumerador máximo
            Case 0
                Return 32
            Case 1
                Return 64
            Case 2
                Return 128
            Case 3
                Return 16
            Case 4
                Return 1
            Case 5
                Return 8
            Case 6
                Return 4
            Case 7
                Return 2
            Case Else
                Return 0
        End Select

    End Function

    'Retorna o indicador da direção de fluxo comparando duas células adjacentes
    Private Function RelativeIncipientFlowDirection(ByVal y1 As Int16, ByVal y2 As Int16, ByVal x1 As Int16, ByVal x2 As Int16) As Int16

        'célula 1 = célula de origem
        'célula 2 = célula de destino

        Dim xRel As Int16 = x2 - x1
        Dim yRel As Int16 = y2 - y1

        If xRel = 1 And yRel = -1 Then Return 128
        If xRel = 1 And yRel = 0 Then Return 1
        If xRel = 1 And yRel = 1 Then Return 2
        If xRel = 0 And yRel = 1 Then Return 4
        If xRel = -1 And yRel = 1 Then Return 8
        If xRel = -1 And yRel = 0 Then Return 16
        If xRel = -1 And yRel = -1 Then Return 32
        If xRel = 0 And yRel = -1 Then Return 64

        'Caso encontre algum outro valor dispara um exception
        Throw New Exception("Problemas ao tentar identificar a direção de fluxo do trecho de rio modificado.")

    End Function

    'Move a linha e a coluna de acordo com o número armazenado
    Private Sub MoveToFlowDirection(ByVal valor As Int16, ByRef Lin As Int16, ByRef col As Int16)

        '32	64	    128
        '16	0	    1      'Configuração das direções de fluxo para o IDRISI Kilimanjaro
        '8	4	    2

        Select Case valor 'Identifica o sentido conforme o numero 

            Case 128
                Lin = Lin - 1
                col = col + 1
            Case 1
                col = col + 1
            Case 2
                Lin = Lin + 1
                col = col + 1
            Case 4
                Lin = Lin + 1
            Case 8
                Lin = Lin + 1
                col = col - 1
            Case 16
                col = col - 1
            Case 32
                Lin = Lin - 1
                col = col - 1
            Case 64
                Lin = Lin - 1
            Case Else
                'Não muda, pois encontrou uma borda/depressão
        End Select
    End Sub

    ''' <summary>
    ''' Escreve a matriz de dados do flow direction
    ''' </summary>
    Public Sub WriteFlowDirectionData(ByVal Arquivo As String)
        _FlowDirection.WriteData(Arquivo)
    End Sub

#End Region

End Class
