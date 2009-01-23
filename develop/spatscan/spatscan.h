/**************************************************************************************
 ****
 **** Archive with global definitions of types of data and functions auxiliary 
 **** used for generation of clusters of arbitrary format from structures 
 **** of space neighborhood
 ****
 ****  Original Author: Andrea Tavares and Marcelo Azevedo Costa
 ****  Date:  25/01/05
 ****  Conversion to surveillance package and translation to english: 
 ****  Michael Höhle and Marcelo Azevedo Costa @ Jan 2009
 ****
 ***************************************************************************************/
#ifndef __SCANCLUSTERH
#define __SCANCLUSTERH


/////////////////////////////////////////////////////////////////////////////////////////
//  Data types
/////////////////////////////////////////////////////////////////////////////////////////

/** Type for areas. They will count coordinate of the centroid, population and the number of cases **/
typedef struct ar {
  int     indice; // Unique identifier. *PO* Varia de 0 ate NumAreas - 1
  double  pop;    // Populacao da area
  double  casos;  // Population of the area
  double  x, y;   // x y coordinates of the area
} TArea;

/** Type for edges between areas, defining the neighborhood graph. They are undirected edges 
 *PO*
 ** direcionadas, que alem do indice de origem e destino, contem ainda um peso **/
/** Tipo para arestas entre areas, definindo o grafo de vizinhanca. Sao arestas nao
 ** direcionadas, que alem do indice de origem e destino, contem ainda um peso   **/
typedef struct link {
  int     area1, area2; //Identificador unico das areas que estao contectadas
  double  peso;         //Peso da aresta, varia de significado de acordo com o algoritmo
} TAresta;


/** Tipo para um cluster. Contem o numero de areas e o identificador das areas no cluster.
 ** Possivelmente podera virar uma classe **/
typedef struct cluster {
  int    tamanho;		// Number of areas in the cluster and the size of the vector indices
  int*   indices;		// Indices of the areas that make up the cluster
  double veross;		// Probability of cluster ***Marcelo (02/02/2005) 
  float  pvalue;	        // p-value ***Marcelo (13/02/2006) 
} TCluster;


/////////////////////////////////////////////////////////////////////////////////////////
//  CONSTANTES E VARIAVES GLOBAIS
/////////////////////////////////////////////////////////////////////////////////////////

#define  MAX_AREAS     5000
#define  MAX_ARESTAS  50000


//extern int      NumAreas;		//Numero de areas total
//extern TArea*   VetAreas;		//Vetor descrevendo todas as areas. Indice do vetor coincide com indice
								//da area.
//extern int      NumArestas;		//Numero total de arestas
//extern TAresta* VetArestas;     //Vetor de arestas do grafo de vizinhaca.

//extern double   TotalPop;		// Total de Populacao na Regiao de estudo	****Marcelo
//extern double   TotalCasos;		// Total de Casos na Regiao de estudo		****Marcelo

/////////////////////////////////////////////////////////////////////////////////////////
//  PROTOTIPOS DE FUNCOES
/////////////////////////////////////////////////////////////////////////////////////////

/** Funcao que calcula a distancia K-L entre as areas de indices id1 e id2.
 ** Parametros
      id1, id2: identificadores das duas areas
      vareas: vetor de descritores das areas
 ** Retorno: distancia K-L
 ** 
 ** A ser implementada pelo Sabino
 **/
extern double DistanciaKL(int id1, int id2, TArea* vAreas);

/** Funcao que calcula a verossimilhanca de um cluster.
 ** Parametros
      clu: apontador para o cluster. O subgrafo induzido pelas areas eh necessariamente conexo.
      vAreas: vetor de descritores das areas.
 ** Retorno: verossimilhanca
 ** 
 ** A ser implementada pelo Marcelo
 **/
extern double VerossimilhancaCluster(TCluster* clu, TArea* vAreas, double TotalPop, double TotalCasos);

/** A fazer: Funcoes para calculo da verossimilhanca para modelo Binomial e de Poisson **/
// extern double BinomialVerossimilhancaCluster(TCluster* clu, TArea* vAreas);
// extern double PoissonVerossimilhancaCluster(TCluster* clu, TArea* vAreas);

/** Funcoes para leitura dos arquivos de dados
 ** Parametros
    str : nome do arquivo a ser lido
	vAreas : vetor de descritores das areas
 ** Retorno: numero de areas ou "0" - erro na operacao
**/
extern int LeCentroides(char *arquivo, TArea** vAreas);	// Deve ser executado primeiro (OBRIGATORIAMENTE)
extern int LePopCasos(char *arquivo, TArea* vAreas, int TAM);

/** Funcao para a Leitura das arestas a partir do arquivo **/
extern int LeArestas(char *arquivo, TAresta** vAresta);

/** Funcao para a Escrita do Cluster em arquivo txt **/
extern int EscreveCluster(char *arquivo, TCluster* clu, double pvlue);

/** Funcao para a Leitura das arestas a partir do arquivo **/
extern int VizinhosCluster(TCluster* vizin, TCluster* clu, TAresta* vAresta, int NumAreas, int NumArestas);

/** Funcao para calculo dos vizinhos de um cluster **/
extern int DoubleVizinhosCluster(TCluster* vizin, TCluster* clu, TAresta* vAresta, int NumAreas, int NumArestas);

/** Funcao para Calculo de Verossimilhanca e agregacao do cluster **/
extern double AgregaAreaCluster(TCluster* clu, TCluster* viz, TArea* VetAreas); // Retorna Verossimilhanca

/** Funcao para Calculo de Verossimilhanca e agregacao do cluster caso tenha aumento
    da verossimilhanca em relação à original quando acrescido um dos vizinhos **/
extern double AgregaAreaCluster2(TCluster* clu, TCluster* viz, TArea* VetAreas); // Retorna Verossimilhanca ou -1

/** Funcao para Calculo de Verossimilhanca e agregacao do cluster caso tenha aumento
    da verossimilhanca em relação à original quando acrescido um dos vizinhos **/
extern double AgregaAreaCluster3(TCluster* clu, TCluster* viz, TArea* VetAreas); // Retorna Verossimilhanca ou 0

/** Funcao para Calculo de Verossimilhanca e agregacao do cluster caso tenha um aumento
    da verossimilhanca de pelo menos o parametro "chi" em relação à original quando 
    acrescido um dos vizinhos **/
extern double AgregaAreaClusterChi(TCluster* clu, TCluster* viz, float chi, TArea* VetAreas);

/** Funcao para reproducao de um cluster **/
extern int CopiaCluster(TCluster* orig, TCluster* dest);

/** Funcao para a geracao de uma distribuicao Multinomial de casos **/
extern int multinomial(long int* vtcasos, double* prob, int tam, int casos);

/** Funcao para a geracao de uma distribuicao Multinomial de casos **/
extern int multinomiald(double* vtcasos, double* prob, int tam, int casos);

/** Funcao para indentificacao do numero de elementos na intersecao **/
extern int Intersection(TCluster* first, TCluster* sec);

/** Funcao para armazenamento do resultados dos clusters primario, secundario e 
    Terceario em arquivo														**/
extern int OutputCluster(char *arquivo, TCluster* best, TCluster* sec, TCluster* third);

/** Funcao para alocacao de memoria para vetor **/
extern double* criavetor(int tam);

/** Funcao para alocacao de memoria para matriz **/
extern double** criamatriz(int rows, int cols);

/** Funcao para liberacao de area alocada para matriz **/
extern void liberamatriz(double** mt, int rows);

/** Funcao para determinacao do cluster candidato e seu p-valor **/
extern int ScanClusterBernoulli(TCluster &clu, long int centroides, double* pop, double* casos, double** geo, long int interacoes, double tx );

extern int dMSTClusterBernoulli(TCluster &clu, long int NumAreas, long int MonteCarlo, TArea*   VetAreas,
						   long int NumArestas, TAresta* VetArestas, long int LimiteAreas, int TamMin, bool doubly);
/** Funcao para detecção de cluster segundo metodo dMST **/




#endif
