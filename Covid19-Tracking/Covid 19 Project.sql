/*
COVID 19 Data Exploration. Data source: https://ourworldindata.org/covid-deaths 
Skills used: Join, CTE, Temp Tables, Windows Functions, Aggregate Functions, Creating Views, Converting Data Types
*/

	--Creating temp table #CD for analysis
drop table if exists #CD
select * into #CD
from CovidDeaths
where continent is not null and date != '2023-02-01'
order by 1,2

	-- Showing list of countries that will be imported into table 'Dim.Country' for visualization
select distinct iso_code, location
from #CD

	-- Total COVID-19 cases and Total deaths by country up to Jan 2023 
	-- Showing coutries with the highest COVID cases and deaths
select location 
	, ISNULL(MAX(total_cases),0) as total_cases
	, ISNULL(MAX(total_deaths),0) as total_deaths
from #CD
--where location = 'Vietnam'
group by location
order by 1, 2 desc

	-- Daily Rolling Total COVID-19 cases and Total deaths 
select location 
	, date 
	, ISNULL(MAX(total_cases),0) as total_cases
	, ISNULL(MAX(total_deaths),0) as total_deaths
from #CD
group by location
		, date
order by 1, 2 desc

	-- Daily new COVID cases and new deaths by country (7-day rolling average)
	-- Using Windows functions
select location, date
	, ISNULL(new_cases,0) as new_cases
	, (SUM(CAST(new_cases as float)) over( partition by location order by date asc rows between 6 preceding and current row))/7 as new_cases_smoothed
	, ISNULL(new_deaths,0) as new_deaths
	, (SUM(CAST(new_deaths as float)) over( partition by location order by date asc rows between 6 preceding and current row))/7 as new_deaths_smoothed
from #CD


	-- Daily new COVID cases and new deaths per million people by country (7-day rolling average) 
	-- Using CTE
with smoothed as
(
select location, date, population
	, ISNULL(new_cases,0) as new_cases
	, (SUM(CAST(new_cases as float)) over( partition by location order by date asc rows between 6 preceding and current row))/7 as new_cases_smoothed
	, ISNULL(new_deaths,0) as new_deaths
	, (SUM(CAST(new_deaths as float)) over( partition by location order by date asc rows between 6 preceding and current row))/7 as new_deaths_smoothed
from #CD
)
select *
	, ISNULL(CAST((new_cases_smoothed/population*1000000) as decimal(10,3)),0) as new_cases_smoothed_per_million
	, ISNULL(CAST((new_deaths_smoothed/population*1000000) as decimal(10,3)),0) as new_deaths_smoothed_per_million
from smoothed
--where date = '2021-01-21' and location like 'United%'

	-- Daily Case Fatality Ratio by country 
	/*rolling-average CFR is calculated as the ratio between the 7-day average number of deaths and the 7-day average number of cases 10 days earlier */
with smoothed as
(
select continent, location, date, population
	, ISNULL(new_cases,0) as new_cases
	, (SUM(CAST(new_cases as float)) over( partition by location order by date asc rows between 6 preceding and current row))/7 as new_cases_smoothed
	, ISNULL(new_deaths,0) as new_deaths
	, (SUM(CAST(new_deaths as float)) over( partition by location order by date asc rows between 6 preceding and current row))/7 as new_deaths_smoothed
from #CD
)
, per_million as (
select *
	, ISNULL(CAST((new_cases_smoothed/population*1000000) as decimal(10,3)),0) as new_cases_smoothed_per_million
	, ISNULL(CAST((new_deaths_smoothed/population*1000000) as decimal(10,3)),0) as new_deaths_smoothed_per_million
from smoothed
)
, lag_10d as (
select *
	, LAG(new_cases_smoothed,10,0) over(partition by location order by date asc) as d10_cases
from per_million 
)
select continent, location, date, population
	, new_cases , new_deaths
	, new_cases_smoothed_per_million, new_deaths_smoothed_per_million
	, ISNULL(CAST((new_deaths_smoothed/NULLIF(d10_cases,0)*100) as decimal(10,2)),0) as case_fatality_ratio
from lag_10d
/* => This above output will be used for PowerBI visualization CovidProject */


	-- LET'S BREAK THINGS DOWN BY CONTINENT
	-- Showing Total COVID cases and deaths by continent

select location as continent
	, MAX(total_deaths) as death_cases
	, MAX(total_cases) as total_cases
	, CAST(ISNULL((MAX(total_deaths)*1.0/MAX(total_cases))*100,0) as decimal(10,2)) as case_fatality_ratio
from CovidDeaths
where continent is null AND location not like '%income'
group by location
order by total_cases desc


	-- GLOBAL NUMBERS 
	-- Showing worldwide daily COVID cases update 

select date
	, ISNULL(SUM(new_deaths),0) as total_deaths
	, ISNULL(SUM(new_cases),0) as total_cases
	, CAST(ISNULL((SUM(new_deaths)*1.0/SUM(new_cases)*100),0) as decimal(10,2)) as case_fatality_ratio
from #CD
group by date
order by date desc

	-- ADD VACCINATIONS INFO
	-- Using Full Join

select *
from CovidDeaths da
join CovidVaccinations va
	on da.location = va.location
	and da.date = va.date

	-- Showing the Rolling number of COVID vaccinations by location

select da.location, da.date
	, ISNULL(va.new_vaccinations,0) as new_vaccinations
	, ISNULL(SUM(CAST(va.new_vaccinations as bigint)) over(partition by da.location order by da.location, da.date),0) as rolling_vaccinations
from CovidDeaths da
join CovidVaccinations va
	on da.location = va.location
	and da.date = va.date
where da.continent is not null
order by location, date desc

	-- Rolling number of vaccinations among population
	-- Using CTE, Join
with vacin as
(select da.location, da.date, da.population
	, ISNULL(va.new_vaccinations,0) as new_vaccinations
	, ISNULL(SUM(CAST(va.new_vaccinations as bigint)) over(partition by da.location order by da.location, da.date),0) as rolling_vaccinations
from CovidDeaths da
join CovidVaccinations va
	on da.location = va.location
	and da.date = va.date
where da.continent is not null
)
select *
	, CAST((rolling_vaccinations*1.0/population*100) as decimal(10,2)) as rolling_vaccinations_among_population
from vacin
order by location, date desc

	-- Creating View for Vizualization

Create View VaccinationsAmongPopulation as

select da.location
	, MAX(population) as population
	, ISNULL(SUM(CAST(va.new_vaccinations as bigint)),0)  as total_vaccinations
	, CAST((ISNULL(SUM(CAST(va.new_vaccinations as bigint)),0)*1.0/ MAX(population)) as decimal(10,2))total_vaccninations_among_population
from CovidDeaths da
join CovidVaccinations va
	on da.location = va.location
	and da.date = va.date
where da.continent is not null
group by da.location
order by 4 desc


