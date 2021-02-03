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
package es.unir.tfm.sipruva.util;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.FormulaEvaluator;

public class Util {

	public static Integer double2Integer(double d) {
		return (int) d;
	}

	public static Double procesaOptionalDouble(FormulaEvaluator formulaEvaluator, Cell cell) {
		if (cell == null || formulaEvaluator.evaluateInCell(cell).getCellType() == CellType.BLANK)
			return null;
		else
			return cell.getNumericCellValue();
	}

	public static Double procesaPossibleValueNotDouble(FormulaEvaluator formulaEvaluator, Cell cell) {
		if (formulaEvaluator.evaluateInCell(cell).getCellType() != CellType.NUMERIC)
			return null;
		else
			return cell.getNumericCellValue();
	}

	public static String formatDate(Date date) {
		SimpleDateFormat format1 = new SimpleDateFormat("yyyy-MM-dd");
		String date1 = format1.format(date);
		return date1;
	}

	
}
