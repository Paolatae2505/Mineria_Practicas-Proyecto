--------------------------
-- Primer Ejercicio ------
--------------------------

CREATE VIEW lista_empleados as
SELECT curp,nombre,materno,paterno
from Empleado;

SELECT * from lista_empleados 
WHERE nombre LIKE 'A%';
