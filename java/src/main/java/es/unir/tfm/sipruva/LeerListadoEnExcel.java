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

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.Row;

import es.unir.tfm.sipruva.model.ListadoEntradaLine;
import es.unir.tfm.sipruva.util.Util;

public class LeerListadoEnExcel {
	
	File excelFile = null;

	private static final int CAMPO1_0 = 0;
	private static final int CAMPO2_1 = 1;
	private static final int CAMPO3_2 = 2;
	private static final int CAMPO4_3 = 3;
	private static final int HORA_4 = 4;
	private static final int ID_SOCIO_5 = 5;
	private static final int TIPO_UVA_6 = 6;
	private static final int NETO_9 = 9;
	private static final int GRADO_10 = 10;
	private static final int KILOGRADO_11 = 11;
	private static final int ACIDEZ_12 = 12;
	private static final int POTASIO_13 = 13;
	private static final int GLUCONICO_14 = 14;
	private static final int TEMPERATURA_15 = 15;
	private static final int CALIDAD_16 = 16;
	private static final int GRADOBRIX_17 = 17;
	private static final int KILOPUNTOS_18 = 18;
	
	public LeerListadoEnExcel(File excelFile) {
		this.excelFile = excelFile;
	}

	public List<ListadoEntradaLine> getLineas() throws Exception {
		// obtenemos el flujo de bytes a partir de la propiedad excelFile de la clase
		FileInputStream fis = new FileInputStream(this.excelFile);
		// creamos instancia HSSFWorkbook
		HSSFWorkbook wb = new HSSFWorkbook(fis);
		// accedemos a la primera hoja de calculo del libro
		HSSFSheet sheet = wb.getSheetAt(0);

		// evaluador del tipo de celda
		FormulaEvaluator formulaEvaluator = wb.getCreationHelper().createFormulaEvaluator();
		Date fecha=null;
		ListadoEntradaLine linea;
		List<ListadoEntradaLine> lineas = new ArrayList<ListadoEntradaLine>();
		int num=0;
		for (Row row : sheet) // recorremos todas las filas de la hoja
		{
			try {
				//para las que son de texto
				if (row.getCell(0)==null ||formulaEvaluator.evaluateInCell(row.getCell(0)).getCellType()==CellType.BLANK||(formulaEvaluator.evaluateInCell(row.getCell(0)).getCellType()==CellType.STRING&&!row.getCell(0).getStringCellValue().toUpperCase().contains("FECHA:")))
					continue;
				//para las que hay que coger la fecha
				if (formulaEvaluator.evaluateInCell(row.getCell(0)).getCellType()==CellType.STRING&&row.getCell(0).getStringCellValue().equals("FECHA:")) {
					fecha=row.getCell(3).getDateCellValue();
					continue;
				}
				//para las que son de anularse, el valor del primer atributo es un 1
				if (row.getCell(0).getNumericCellValue()==1) {
					continue;
				}
				
				linea = new ListadoEntradaLine();
				linea.setFecha(Util.formatDate(fecha));
				linea.setCampo1(Util.double2Integer(row.getCell(CAMPO1_0).getNumericCellValue()));
				linea.setCampo2(Util.double2Integer(row.getCell(CAMPO2_1).getNumericCellValue()));
				linea.setCampo3(Util.double2Integer(row.getCell(CAMPO3_2).getNumericCellValue()));
				linea.setCampo4(Util.double2Integer(row.getCell(CAMPO4_3).getNumericCellValue()));
				linea.setHora(row.getCell(HORA_4).getStringCellValue());
				linea.setIdSocio(row.getCell(ID_SOCIO_5).getStringCellValue());
				linea.setTipoUva(row.getCell(TIPO_UVA_6).getStringCellValue().trim());
				linea.setNeto(Util.double2Integer(row.getCell(NETO_9).getNumericCellValue()));
				linea.setGrado(row.getCell(GRADO_10).getNumericCellValue());
				linea.setKilogrado(Util.double2Integer(row.getCell(KILOGRADO_11).getNumericCellValue()));
				linea.setAcidez(Util.procesaOptionalDouble(formulaEvaluator, row.getCell(ACIDEZ_12)));
				linea.setPotasio(Util.procesaOptionalDouble(formulaEvaluator, row.getCell(POTASIO_13)));
				linea.setGluconico(Util.procesaPossibleValueNotDouble(formulaEvaluator, row.getCell(GLUCONICO_14)));
				linea.setTemperatura(Util.procesaOptionalDouble(formulaEvaluator, row.getCell(TEMPERATURA_15)));
				linea.setCalidad(Util.double2Integer(row.getCell(CALIDAD_16).getNumericCellValue()));
				linea.setGradoBrix(Util.procesaOptionalDouble(formulaEvaluator, row.getCell(GRADOBRIX_17)));
				linea.setKilopuntos(Util.procesaOptionalDouble(formulaEvaluator, row.getCell(KILOPUNTOS_18)));
				
				lineas.add(linea);
				num+=1;
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println(num+"|"+row.getCell(0)+"|"+row.getCell(1)+"|"+row.getCell(2)+"|"+row.getCell(3)+"|"+row.getCell(4)+"|"+row.getCell(5)+"|"+row.getCell(6)+"|"+row.getCell(7)+"|"+row.getCell(8)+"|"+row.getCell(9)+"|"+row.getCell(10)+"|"+row.getCell(11)+"|"+row.getCell(12)+"|"+row.getCell(13)+"|"+row.getCell(14)+"|"+row.getCell(15)+"|"+row.getCell(16)+"|"+row.getCell(17)+"|"+row.getCell(18));
				throw new Exception(e);
			} finally {
				wb.close();
			}
	
		}
		return lineas;
	}
}
