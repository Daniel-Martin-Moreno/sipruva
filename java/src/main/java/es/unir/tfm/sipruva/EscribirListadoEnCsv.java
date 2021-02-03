/*************************************************************************
*Copyright 2021 Daniel Martín-Moreno Romero
*
*Licensed under the Apache License, Version 2.0 (the "License");
*you may not use this file except in compliance with the License.
*You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
*Unless required by applicable law or agreed to in writing, software
*distributed under the License is distributed on an "AS IS" BASIS,
*WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*See the License for the specific language governing permissions and
*limitations under the License.
*************************************************************************/
package es.unir.tfm.sipruva;

import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import org.supercsv.cellprocessor.Optional;
import org.supercsv.cellprocessor.ParseDouble;
import org.supercsv.cellprocessor.ParseInt;
import org.supercsv.cellprocessor.ift.CellProcessor;
import org.supercsv.io.CsvBeanWriter;
import org.supercsv.io.ICsvBeanWriter;
import org.supercsv.prefs.CsvPreference;

import es.unir.tfm.sipruva.model.ListadoEntradaLine;

public class EscribirListadoEnCsv {
	
    private static CellProcessor[] getProcessors() 
    {
 
        final CellProcessor[] processors = new CellProcessor[] {
                new Optional(), // fecha
                new Optional(new ParseInt()), // campo1
                new Optional(new ParseInt()), // campo2
                new Optional(new ParseInt()), // campo3
                new Optional(new ParseInt()), // campo4
                new Optional(), // hora
                new Optional(), // id_socio
                new Optional(), // tipo_uva
                new Optional(new ParseInt()), //neto
                new Optional(new ParseDouble()), // grado
                new Optional(new ParseInt()), // kilogrado
                new Optional(new ParseDouble()), // acidez
                new Optional(new ParseDouble()), // potasio
                new Optional(new ParseDouble()), // gluconico
                new Optional(new ParseDouble()), // temperatura
                new Optional(new ParseInt()), //calidad
                new Optional(new ParseDouble()), //gradoBrix
                new Optional(new ParseDouble()), //kilopuntos
        };
        return processors;
    }
     
    public static boolean write(List<ListadoEntradaLine> lineas, String file) 
    {
         
        ICsvBeanWriter beanWriter = null;
        try
        {
            beanWriter = new CsvBeanWriter(new FileWriter(file), CsvPreference.STANDARD_PREFERENCE);
            final String[] header = new String[] { "fecha", "campo1", "campo2", "campo3" ,"campo4", "hora", "idSocio", "tipoUva", "neto", "grado", "kilogrado", "acidez", "potasio", "gluconico", "temperatura", "calidad", "gradoBrix", "kilopuntos" };
 
            final CellProcessor[] processors = getProcessors();
 
            // escribe el encabezado
            beanWriter.writeHeader(header);
 
            // escribe los beans de datos
            for (ListadoEntradaLine c : lineas) {
                beanWriter.write(c, header, processors);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }  finally {
            try {
                beanWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return true;
    }

}
