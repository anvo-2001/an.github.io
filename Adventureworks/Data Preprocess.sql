  -- Cleaning DimProduct Table --
SELECT [ProductID]
      ,t1.[Name] as [Product Name]
      --,[ProductNumber]
      --,[MakeFlag]
      --,[FinishedGoodsFlag]
      --,[Color]
      --,[SafetyStockLevel]
      --,[ReorderPoint]
      ,[StandardCost]
      --,[ListPrice]
      --,[Size]
      --,[SizeUnitMeasureCode]
      --,[WeightUnitMeasureCode]
      --,[Weight]
      --,[DaysToManufacture]
      --,[ProductLine]
      --,[Class]
      --,[Style]
      ,t1.[ProductSubcategoryID]
	  ,t2.[Name] as [Product Subcategory] 
	  ,t3.[ProductCategoryID]
	  ,t3.[Name] as [Product Category]
      --,[ProductModelID]
      --,[SellStartDate]
      --,[SellEndDate]
      --,[DiscontinuedDate]
      --,[rowguid]
      --,[ModifiedDate]
FROM [AdventureWorks2022].[Production].[Product] t1
LEFT JOIN [Production].[ProductSubcategory]  t2 on t1.ProductSubcategoryID = t2.ProductSubcategoryID
LEFT JOIN [Production].[ProductCategory] t3 on t2.ProductCategoryID = t3.ProductCategoryID

-- Cleaning Dim SalesTerritory Table --
SELECT [TerritoryID]
      ,[Name] Region
      ,[CountryRegionCode] as Country
      ,[Group] as Continent
      --,[SalesYTD]
      --,[SalesLastYear]
      --,[CostYTD]
      --,[CostLastYear]
      --,[rowguid]	
      --,[ModifiedDate]
  FROM [AdventureWorks2022].[Sales].[SalesTerritory]

 -- Fact SalesOrderHeader--
SELECT [SalesOrderID]
      --,[RevisionNumber]
      ,[OrderDate]
      --,[DueDate]
      --,[ShipDate]
      --,[Status]
      --,[OnlineOrderFlag]
      --,[SalesOrderNumber]
      --,[PurchaseOrderNumber]
      --,[AccountNumber]
      ,[CustomerID]
      --,[SalesPersonID]
      ,[TerritoryID]
      --,[BillToAddressID]
      --,[ShipToAddressID]
      --,[ShipMethodID]
      --,[CreditCardID]
      --,[CreditCardApprovalCode]
      --,[CurrencyRateID]
      --,[SubTotal]
      --,[TaxAmt]
      --,[Freight]
      ,[TotalDue]
      --,[Comment]
      --,[rowguid]
      --,[ModifiedDate]
  FROM [AdventureWorks2022].[Sales].[SalesOrderHeader]

-- Fact SalesOrderDetail
SELECT [SalesOrderID]
      ,[SalesOrderDetailID]
      --,[CarrierTrackingNumber]
      ,[OrderQty]
      ,t0.[ProductID]
      --,[SpecialOfferID]
      --,[UnitPrice]
      --,[UnitPriceDiscount]
      ,[LineTotal]
	  ,[LineTotal] - OrderQty*StandardCost as [ProfitMargin]
      --,[rowguid]
      --,[ModifiedDate]
  FROM [AdventureWorks2022].[Sales].[SalesOrderDetail] t0
  LEFT JOIN [Production].[Product] t1 on t0.ProductID = t1.ProductID

-- Customer Retention
with customer_profile as (
SELECT customerid
    ,cast ( OrderDate as date) as transaction_day
    ,min(cast (OrderDate as date)) over (PARTITION BY customerid) as first_day
from Sales.SalesOrderHeader
)
select *
    , datediff(day, first_day, transaction_day) as subsequent_day
    , case when datediff(day, first_day, transaction_day) = 0 then 'New Customers'
    else 'Retained Customers' end as Retention
from customer_profile

-- Customer Segmentation
--CREATE OR ALTER VIEW dbo.vw_CustomerSegmentation AS
WITH customer_profile AS (
SELECT CustomerID
    ,cast ( dateadd(year,10,OrderDate) as date) as TransactionDay
    ,MAX(dateadd(year,10,OrderDate)) over (partition by CustomerID) as Recency
    ,TotalDue
FROM Sales.SalesOrderHeader
WHERE dateadd(year,10,OrderDate) > '2023-01-01' and dateadd(year,10,OrderDate) < '2024-01-01'
)
, rfm as (--Step 1: Define r,f,m for each customer
SELECT CustomerID
    , MIN(DATEDIFF(day, Recency,'2023-12-31')) as Recency
    , COUNT(*) as Frequency
    , SUM(TotalDue) as Monetary
FROM customer_profile
GROUP BY CustomerID
)
, rfm_rank as (--Step2: Assign percentile rank to each r,f,m of each customer
SELECT *
    , PERCENT_RANK() OVER ( ORDER BY Recency ASC ) AS r_percent_rank
    , PERCENT_RANK() OVER ( ORDER BY Frequency DESC ) AS f_percent_rank
    , PERCENT_RANK() OVER ( ORDER BY Monetary DESC ) AS m_percent_rank
FROM rfm
)
, rfm_tier AS ( 
SELECT *
    , CASE WHEN r_percent_rank > 0.75 THEN 4
        WHEN r_percent_rank > 0.5 THEN 3
        WHEN r_percent_rank > 0.25 THEN 2
        ELSE 1 END AS r_tier
    , CASE WHEN f_percent_rank > 0.75 THEN 4
        WHEN f_percent_rank > 0.5 THEN 3
        WHEN f_percent_rank > 0.25 THEN 2
        ELSE 1 END AS f_tier
    , CASE WHEN m_percent_rank > 0.75 THEN 4
        WHEN m_percent_rank > 0.5 THEN 3
        WHEN m_percent_rank > 0.25 THEN 2
        ELSE 1 END AS m_tier
FROM rfm_rank
)
, rfm_group AS ( 
    SELECT * 
        , CONCAT(r_tier, f_tier, m_tier) AS rfm_score
    FROM rfm_tier
) -- Step 3: Grouping these customers based on segmentation rules
, segment_table AS (
SELECT *
    , CASE 
        WHEN rfm_score  =  111 THEN 'Best Customers'
        WHEN rfm_score LIKE '[3-4][3-4][1-4]' THEN 'Lost Bad Customer'
        WHEN rfm_score LIKE '[3-4]2[1-4]' THEN 'Lost Customers'
        WHEN rfm_score LIKE  '21[1-4]' THEN 'Almost Lost'  
        WHEN rfm_score LIKE  '11[2-4]' THEN 'Loyal Customers'
        WHEN rfm_score LIKE  '[1-2][1-3]1' THEN 'Big Spenders' 
        WHEN rfm_score LIKE  '[1-2]4[1-4]' THEN 'New Customers'  
        WHEN rfm_score LIKE  '[3-4]1[1-4]' THEN 'Hibernating' 
        WHEN rfm_score LIKE  '[1-2][2-3][2-4]' THEN 'Potential Loyalists' 
    ELSE 'unknown'
    END AS Segment
FROM rfm_group
)
SELECT Segment
    , COUNT(CustomerID) AS NumberUsers 
    , SUM(COUNT(CustomerID)) OVER() AS TotalUsers
    , 1.0*COUNT(CustomerID) / SUM( COUNT(CustomerID)) OVER() as Pct
FROM segment_table
GROUP BY Segment
