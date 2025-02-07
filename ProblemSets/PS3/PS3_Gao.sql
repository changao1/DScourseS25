DROP TABLE IF EXISTS florida_insurance;

CREATE TABLE florida_insurance (
    policyID INTEGER,
    statecode VARCHAR(2),
    county VARCHAR(50),
    eq_site_limit DECIMAL(15,2),
    hu_site_limit DECIMAL(15,2),
    fl_site_limit DECIMAL(15,2),
    fr_site_limit DECIMAL(15,2),
    tiv_2011 DECIMAL(15,2),
    tiv_2012 DECIMAL(15,2),
    eq_site_deductible DECIMAL(15,2),
    hu_site_deductible DECIMAL(15,2),
    fl_site_deductible DECIMAL(15,2),
    fr_site_deductible INTEGER,
    point_latitude DECIMAL(10,6),
    point_longitude DECIMAL(10,6),
    line VARCHAR(50),
    construction VARCHAR(50),
    point_granularity INTEGER
);

-- read the csv
LOAD DATA LOCAL INFILE 'FL_insurance_sample.csv' 
INTO TABLE florida_insurance 
FIELDS TERMINATED BY ',' 
LINES TERMINATED BY '\n' 
IGNORE 1 LINES;

-- print out the first 10 rows
SELECT * FROM florida_insurance LIMIT 10;

-- list which counties are in the sample
SELECT DISTINCT county FROM florida_insurance ORDER BY county;

-- compute the average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) as avg_appreciation FROM florida_insurance;

-- create a frequency table of the construction variable
SELECT 
    construction,
    COUNT(*) as count,
    COUNT(*) * 100.0 / (SELECT COUNT(*) FROM florida_insurance) as percentage
FROM florida_insurance
GROUP BY construction
ORDER BY count DESC;