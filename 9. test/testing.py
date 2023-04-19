# Testing Exercise for Reproducible Research class
# Created by Kunhong Yu(444447)

import unittest

def convert_temperature(F_degree, target = 'C_degree'):
	"""
	Convert temperature in Fahrenheit degrees to Celsius degrees or Kelvins
	Args :
		--F_degree: Fahrenheit degree
		--target: default is 'C_degree' with Celsius degree, or Kelvin being 'K_degree'
	return:
		--temp: converted temperature
	"""
	assert target in ['C_degree', 'K_degree'], "target must be either 'C_degree'(Celsius degree) or 'K_degree'(Kelvin degree)"
	if target == 'C_degree': # Celsius
		temp = (F_degree - 32.) * 5. / 9.
	else: # Kelvin
		temp = (F_degree - 32.) * 5. / 9. + 273.15

	return round(temp, 3)

class TestTemperature(unittest.TestCase):
	"""Define testing temperature test class"""
	def test_F_to_C(self):
		"""Testing F_degree to C_degree"""
		for f, real_c in zip([50, 70, 90], [10, 21.111, 32.222]):
			self.assertEqual(convert_temperature(f, target = 'C_degree'), real_c)

	def test_F_to_K(self):
		"""Testing F_degree to K_degree"""
		for f, real_k in zip([-500, 0, 1000], [-22.406, 255.372, 810.928]):
			self.assertEqual(convert_temperature(f, target = 'K_degree'), real_k)

	def test_wrong_target(self):
		"""Testing if raising error when passing wrong target"""
		with self.assertRaises(AssertionError):
			convert_temperature(10, target = 'R_degree') # Réaumur


# start testing
if __name__ == '__main__':
	unittest.main()
