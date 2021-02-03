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
package es.unir.tfm.sipruva.model;

public class ListadoEntradaLine {
	
	private String fecha;
	private Integer campo1;
	private Integer campo2;
	private Integer campo3;
	private Integer campo4;
	private String hora;
	private String idSocio;
	private String tipoUva;
	private Integer neto;
	private Double grado;
	private Integer kilogrado;
	private Double acidez;
	private Double potasio;
	private Double gluconico;
	private Double temperatura;
	private Integer calidad;
	private Double gradoBrix;
	private Double kilopuntos;
	
	public ListadoEntradaLine() {
	}
	
	public ListadoEntradaLine(String fecha, Integer campo1, Integer campo2, Integer campo3, Integer campo4, String hora,
			String idSocio, String tipoUva, Integer neto, Double grado, Integer kilogrado, Double acidez,
			Double potasio, Double gluconico, Double temperatura, Integer calidad, Double gradoBrix,
			Double kilopuntos) {
		super();
		this.fecha = fecha;
		this.campo1 = campo1;
		this.campo2 = campo2;
		this.campo3 = campo3;
		this.campo4 = campo4;
		this.hora = hora;
		this.idSocio = idSocio;
		this.tipoUva = tipoUva;
		this.neto = neto;
		this.grado = grado;
		this.kilogrado = kilogrado;
		this.acidez = acidez;
		this.potasio = potasio;
		this.gluconico = gluconico;
		this.temperatura = temperatura;
		this.calidad = calidad;
		this.gradoBrix = gradoBrix;
		this.kilopuntos = kilopuntos;

	}
	
	public String getFecha() {
		return fecha;
	}
	public void setFecha(String fecha) {
		this.fecha = fecha;
	}
	public Integer getCampo1() {
		return campo1;
	}
	public void setCampo1(Integer campo1) {
		this.campo1 = campo1;
	}
	public Integer getCampo2() {
		return campo2;
	}
	public void setCampo2(Integer campo2) {
		this.campo2 = campo2;
	}
	public Integer getCampo3() {
		return campo3;
	}
	public void setCampo3(Integer campo3) {
		this.campo3 = campo3;
	}
	public Integer getCampo4() {
		return campo4;
	}
	public void setCampo4(Integer campo4) {
		this.campo4 = campo4;
	}
	public String getHora() {
		return hora;
	}
	public void setHora(String hora) {
		this.hora = hora;
	}
	public String getIdSocio() {
		return idSocio;
	}
	public void setIdSocio(String idSocio) {
		this.idSocio = idSocio;
	}
	public String getTipoUva() {
		return tipoUva;
	}
	public void setTipoUva(String tipoUva) {
		this.tipoUva = tipoUva;
	}
	public Integer getNeto() {
		return neto;
	}
	public void setNeto(Integer neto) {
		this.neto = neto;
	}
	public Double getGrado() {
		return grado;
	}
	public void setGrado(Double grado) {
		this.grado = grado;
	}
	public Integer getKilogrado() {
		return kilogrado;
	}
	public void setKilogrado(Integer kilogrado) {
		this.kilogrado = kilogrado;
	}
	public Double getAcidez() {
		return acidez;
	}
	public void setAcidez(Double acidez) {
		this.acidez = acidez;
	}
	public Double getPotasio() {
		return potasio;
	}
	public void setPotasio(Double potasio) {
		this.potasio = potasio;
	}
	public Double getGluconico() {
		return gluconico;
	}
	public void setGluconico(Double gluconico) {
		this.gluconico = gluconico;
	}
	public Double getTemperatura() {
		return temperatura;
	}
	public void setTemperatura(Double temperatura) {
		this.temperatura = temperatura;
	}
	public Integer getCalidad() {
		return calidad;
	}
	public void setCalidad(Integer calidad) {
		this.calidad = calidad;
	}
	public Double getGradoBrix() {
		return gradoBrix;
	}
	public void setGradoBrix(Double gradoBrix) {
		this.gradoBrix = gradoBrix;
	}
	public Double getKilopuntos() {
		return kilopuntos;
	}
	public void setKilopuntos(Double kilopuntos) {
		this.kilopuntos = kilopuntos;
	}
	

}
