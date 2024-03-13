# IMIPAS-DATASET

```bash
for i in $(seq 5 9); do echo curl -OJX GET https://nube.conapesca.gob.mx/sites/cona/dgppe/anuarios/BASE_DE_DATOS_DE_PRODUCCION_ANUARIO_200${i}.zip; done | sh
for i in $(seq 10 23); do echo curl -OJX GET https://nube.conapesca.gob.mx/sites/cona/dgppe/anuarios/BASE_DE_DATOS_DE_PRODUCCION_ANUARIO_20${i}.zip; done | sh
for i in $(ls *zip); do unzip $i; done

mkdir XLS
mv *xlsx XLS/

```
