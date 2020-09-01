  " 	 	<H3> ESPECIFICO DEL PAÍS </H3>
		<H4>Fuente de datos</H4>
		<p>Los datos de COVID-19 respecto los casos diagnósticados y la mortalidad desde el 1 de Enero del 2020 hasta la fecha se encuentran en el <a href='https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide'>European Centre for Disease Prevention and Control geographical distribution of COVID-19 cases worldwide (ECDC, 2020).</a> </p>
				<br>              

		<H4>Análisis de tendencia y proyecciones</H4>
                <p>Para analizar la tendencia y obtener proyecciones a tres días, se utiliza un modelo de regresión de Poisson (Dyba and Hakulinen, 2000), corregido por sobredispersión (Navarro et al., 2001), ajustando términos lineal, cuadrático y cúbico de tendencia diaria:</p>
 
		$$\\log(E(c_t))= \\beta_0 + \\beta_1  t + \\beta_2  t^2 + \\beta_3  t^3$$
      
		<p>donde \\(t=1,2,\\ldots,T\\) representa la unidad temporal (desde el primer día hasta el último observado, \\(T\\) días consecutivos en total) y \\(c_t\\) el número de eventos diarios.</p>
		<p>Las proyecciones a tres días, y su intervalo de confianza al 95%, se obtienen a partir de los parámetros estimados por el modelo. También se calculan las tendencias y proyecciones por grupos de edad (0-39, 40-49, 50-59, 60-69, 70-79 y 80 o más años).</p>
		<p>En versiones previas de la aplicación también se consideró un modelo alternativo, incluyendo únicamente la tendencia lineal o la tendecia lineal y la cuadrática. Los modelos se comparaban mediante una prueba de razón de similitudes. A partir de la evolución de la epidemia, observamos que el mejor ajuste lo proporciaba el modelo cúbico, descrito anteriormente, por lo que es el modelo que se utiliza en la versión actual. Excepto par los casos y la mortalidad incidentes que se utiliza el modelo cuadrático. En cualquier caso, la bondad de ajuste de los modelos se evalúa regularmente en caso de ser necesario una reformulación que pudiese proporcionar un mejor ajuste de los datos durante el transcurso de la epidemia.</p>
		<br>
		<H4>Anàlisis de letalidad</H4>
		<p>La tasa de letalidad se define como el cociente entre los fallecidos y los casos diagnosticados (Rothman and Greenland, 1998). Para evaluar la tendencia se utiliza un modelo de regresión de Poisson (Dyba and Hakulinen, 2000), corregido por sobredispersión (Dyba and Hakulinen, 2000), ajustando términos lineal, cuadrático y cúbico de tendencia diaria e incluyendo un término offset con los casos diagnosticados:</p>

		$$\\log(E(m_t))= \\beta_0 + \\beta_1  t + \\beta_2  t^2 + \\beta_3  t^3 + \\log(c_t)$$
      
		<p>donde \\(m_t\\) representa el número diario de pacientes fallecidos, y \\(c_t\\) los casos diagnósticados. También se calculan las tasas de letalidad por los mismos grupos de edad.</p>
		<p>En la actualidad, no es posible realizar un estimación precisa de la tasa de letalidad debido al grado de subregistro de los casos diagnósticados en las estadísticas oficiales (Battegay et al., 2020). Si bien es de interés su estimación y seguimiento considerando esta limitación.</p>
		<br>
		
		<H4>Anàlisis del momento de infección estimado</H4>
		<p>Lauer et al. (Lauer et al., 2020) han analizado recientemente el periodo de incubación de la COVID-19 en una cohorte de pacientes sintomáticos. De cada paciente recogieron el intervalo de exposición al SARS-CoV-2 y la fecha de aparición de síntomas. Asumieron que el tiempo de incubación seguiría, como en otras infecciones virales del tracto respiratorio, una distribución Lognormal.</p>

		$$\\ Lognormal(\\mu,\\sigma^2) = Lognormal(1.621 , 0.418)$$

		<p>En un ejercicio naif y no exento de limitaciones, hemos replicado esta distribución en el conjunto de casos diagnosticados para aproximar la fecha de exposición al SARS-CoV-2 de forma recursiva:</p>

		$$\\ q(i)= \\sum_{j=1}^{14} P(j)*c_{j+i}$$

		<p>siendo \\(q\\) el número de casos infectados en la fecha \\(i\\), \\(c\\) el número de casos diagnosticados en la fecha \\(i+j\\), y \\(P(j)\\) es la probabilidad de presentar síntomas el día \\(j\\) según una ley Lognormal con los parámetros definidos por Lauer et al. (Lauer et al., 2020)
		Para la estimación de los últimos 14 días, al no disponer de la información de los casos diagnósticados para los siguientes 14, se ha utilizado el modelo cuadrático descrito con anterioridad para la proyección de casos diagnosticados. En la aplicación se muestran estas últimas estimaciones de un color distinto. </p>
		<br>
		<H4>Evolución índice R</H4>
			<p>El número de reproducción, \\(R\\), es el valor promedio de segundas infecciones causadas por un solo individuo infectado a lo largo de su periodo infeccioso. Este estadístico, que varia según el tiempo y la situación, es utilizado para caracterizar la transmisibilidad de un patógeno en una epidemia. La monitorización de \\(R\\) en el tiempo te da una posible lectura de la efectividad de las intervenciones y de la necesidad de intensificar las medidas de control, dado que el objetivo de estas es reducir \\(R\\) por debajo del valor lindar de 1 y lo más cerca de 0 posible, momento en el que estaría la epidemia bajo control.</p>
		<p>Aquí hemos utilizado el paquete EpiEstim para obtener el número de reproducción \\(R_t\\) utilizando el método de Wallinga y Teunis. Este método paramétrico parte de una distribución gamma para el intervalo de serie. El intervalo de serie es el tiempo entre la aparición de síntomas en el primer caso y la aparición de síntomas en el segundo, que se necesita para estimar \\(R_t\\) en el curso de una epidemia. </p>
		<p> La media y la desviación estándar de la distribución del intervalo de serie puede variar entre enfermedades. Para el sarampión se estimó una media (SD) de 14.9 (3.9) días (Groendyke et al.); para la influenza pandémica 2.6 (1.5) (Ferguson et al.; Boelle et al.); para la viruela 22.4 (6.1) (Riley and Ferguson); para el SARS 8.4 (3.8) (Lipsitch et al.).</p>
		<p>En un artículo reciente de Nishiura et al. (Serial interval of novel coronavirus (COVID-19) infections. Int J Infect Dis. 2020 Mar 4;93:284-286) se estimó una media y una desviación estándar para el COVID-19 de 4.7 días (95% CrI: 3.7, 6.0) y 2.9 días (95% CrI: 1.9, 4.9) respectivamente, que es mucho más corto que el del SARS i el MERS. Estos son los valores, 4.7 (2.9), que utilizamos en nuestro análisis. </p>
		<p> Para el cálculo se ha utilizado el paquete de R EpiEstim, desenvolupado por Cori et al (American Journal of Epidemiology, 2013).</p>
		<br>
		<H4>Limitaciones </H4>
		
		<p>Las estimaciones no son exactas debido a la infradeclaración de casos diagnosticados y de mortalidad en las estadísticas oficiales (<a href='https://www.ncbi.nlm.nih.gov/pubmed/32031234'>Battegay et al. 2020</a>). </p>
		
		<p>La selección del número de personas que se han hecho una prueba diagnóstica es crucial para una estimación precisa (<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Pearce et al. 2020</a>). La precisión de la estimación de las tasas depende de la estrategia en hacer los tests, de la prevalencia de la infección y de la precisión de los tests. Diferencias entre países pueden significar solamente diferencias en la selección a la hora de hacer tests y del funcionamiento de los mismos.(<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Pearce et al. 2020</a>).</p> 
		
		<p>La aplicación no tiene en cuenta los cambios en la definición de casos diagnosticados ni las mesuras de confinamiento contra el coronavirus que se han llevado a cabo en los distintos países con la intención de aplanar la curva.</p>
		
		<H4>Referencias</H4>
		<ul>
				<li>Battegay M, Kuehl R, Tschudin-Sutter S, Hirsch HH, Widmer AF, Neher RA.<a href='https://www.ncbi.nlm.nih.gov/pubmed/32031234'>2019-novel Coronavirus (2019-nCoV): estimating the case fatality rate - a word of caution</a>. Swiss Med Wkly. 2020;150:w20203.</li>
		<li>Cori A, Ferguson NM, Fraser C, Cauchemez S. <a href='https://www.ncbi.nlm.nih.gov/pubmed/24043437'>A new framework and software to estimate time-varying reproduction numbers during epidemics</a>. Am J Epidemiol. 2013;178(9):1505-12.</li>

		<li>Dyba T, Hakulinen T. <a href='https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20000715)19:13<1741::AID-SIM496>3.0.CO;2-O'> Comparison of different approaches to incidence prediction based on simple interpolation techniques</a>. Statistics in Medicine 2000; 19: 1741-1752.</li>
					<li>European Centre for Disease Prevention and Control. Download today’s data on the geographic distribution of COVID-19 cases worldwide Stockholm. Available from: <a href='https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide/'> https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide/ </a>.</li>
						<li>Lauer SA, Grantz KH, Bi Q, Jones FK, Zheng Q, Meredith HR, et al. <a href='https://www.acpjournals.org/doi/10.7326/M20-0504'> The Incubation Period of Coronavirus Disease 2019 (COVID-19) From Publicly Reported Confirmed Cases: Estimation and Application </a>. Ann Intern Med 2020.</li>
			<li>Navarro A, Utzet F, Puig P, Caminal J, Martín M. <a href='https://www.gacetasanitaria.org/es-la-distribucion-binomial-negativa-frente-articulo-resumen-S0213911101715993'> La distribución binomial negativa frente a la de Poisson en el análisis de fenómenos recurrentes</a>. Gaceta Sanitaria 2001; 15: 447-452.</li>
						<li>Pearce N, Vandenbroucke JP, VanderWeele TJ, Greenland S.<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Accurate Statistics on COVID-19 Are Essential for Policy Guidance and Decisions</a>. Am J Public Health. 2020:e1-e3.</li>
			<li>Rothman K, Greenland S. Modern epidemiology. Philadelphia, PA: Lippincott-Raven Publishers, 1998.</li>

		</ul>
  
  "