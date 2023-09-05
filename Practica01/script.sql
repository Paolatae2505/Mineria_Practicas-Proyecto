--------------------------
-- Primer Ejercicio ------
--------------------------

CREATE VIEW lista_empleados as
SELECT curp,nombre,materno,paterno
from Empleado;

SELECT * from lista_empleados 
WHERE nombre LIKE 'A%';

--------------------------
-- Tercer Ejercicio ------
--------------------------

CREATE VIEW ColaboracionProyecto AS
SELECT Proyecto.numProy, Proyecto.nombreProy, COUNT(Colaborar.curp) as numEmpleados, sum(Colaborar.numHoras) as numHoras
FROM Proyecto JOIN Colaborar ON Proyecto.numProy = Colaborar.numProy GROUP BY Proyecto.numProy,  Proyecto.nombreProy;

---------------------------------------
--------- Ejercicio 4 -----------------
---------------------------------------

-- lista_empleados --------------------

-- No se puede insertar el valor NULL en la columna 'numDepto'. La columna no admite valores NULL. 
INSERT INTO lista_empleados (curp, nombre, materno, paterno)
VALUES ('BAHV981101MDFRRC03', 'Victoria', 'Herrera', 'Baron');

-- Borró de tabla empleado y por consiguiente, de la vista
DELETE FROM lista_empleados
WHERE curp = 'ACWU948539FDBQAR18';

-- Funcionó
UPDATE lista_empleados
SET materno = 'Baron'
WHERE curp = 'BNMR859072HIBZAN70';

------- ColaboracionProyecto ----------

-- No puede actualizar ni insertar la vista o función 'ColaboracionProyecto' porque contiene un campo derivado o constante.
INSERT INTO ColaboracionProyecto (numProy, nombreProy, numEmpleados, numHoras)
VALUES (502, 'ProyectoNuevo', 5, 100);

-- La vista o función 'ColaboracionProyecto' no es actualizable porque la modificación afecta a varias tablas base.
DELETE FROM ColaboracionProyecto
WHERE numProy = 1;

-- No puede actualizar ni insertar la vista o función 'ColaboracionProyecto' porque contiene un campo derivado o constante.
UPDATE ColaboracionProyecto
SET numEmpleados = 20
WHERE numProy = 1;


